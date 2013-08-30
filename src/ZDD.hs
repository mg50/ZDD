{-# LANGUAGE BangPatterns #-}
module ZDD where
import Prelude hiding (lookup)
import qualified Data.Map as M
import Data.Maybe
import Control.Monad

data ZNode a = ZNode { value :: a
                     , hiId :: Int
                     , loId :: Int
                     }
               | Top
               | Bottom
               deriving (Eq, Show, Ord)

data ZRecord a = ZRecord a Int Int
               | TopRec
               | BotRec deriving (Eq, Ord, Show)

data ZStore a = ZStore { counter :: !Int
                       , nodesById :: M.Map Int (ZNode a)
                       , idsByNode :: M.Map (ZNode a) Int
                       } deriving (Show, Eq)

lookupById :: Int -> ZStore a -> ZNode a
lookupById id store = case M.lookup id (nodesById store) of
  Just node -> node
  Nothing   -> error $ "could not find node with id: " ++ show id


lookupByNode :: (Ord a) => ZNode a -> ZStore a -> Int
lookupByNode node store = case M.lookup node (idsByNode store) of
  Just id -> id
  Nothing -> error $ "could not find id for node"

emptyZStore :: (Ord a) => ZStore a
emptyZStore = ZStore 2 a b
  where a = M.fromList [(0, Bottom), (1, Top)]
        b = M.fromList [(Bottom, 0), (Top, 1)]

insert :: (Ord a) => ZNode a -> ZStore a -> ZStore a
insert zn zst@(ZStore ctr n i) = case M.lookup zn (idsByNode zst)  of
  Just _  -> zst
  Nothing -> ZStore ctr' n' i'
               where ctr' = ctr + 1
                     n' = M.insert ctr zn n
                     i' = M.insert zn ctr i

reduce :: (Ord a) => ZStore a -> ZNode a -> ZNode a
reduce zst zn = if hiId zn == 0
                   then case M.lookup (loId zn) (nodesById zst) of
                          Just node -> node
                          Nothing   -> error $ "could not find node with id: " ++ show (loId zn)
                   else zn

toList :: (Ord a) => ZStore a -> Int -> [[a]]
toList store 0 = []
toList store 1 = [[]]
toList store id = case M.lookup id (nodesById store) of
  Just (ZNode val hiId loId) -> map (val:) (toList store hiId) ++ toList store loId
  Nothing -> error $ "could not find node with id: " ++ show id

union :: (Ord a) => ZStore a -> ZNode a -> ZNode a -> (ZNode a, ZStore a)
union store Bottom node = (node, store)
union store node Bottom = (node, store)
union store Top Top = (Top, store)
union store Top Bottom = (Top, store)
union store (ZNode v hiId loId) Top = let lo = lookupById loId store
                                          (lo', store') = union store lo Top
                                          loId' = lookupByNode lo' store
                                          node' = ZNode v hiId loId'
                                      in (node', insert node' store')
union store Top node = union store node Top



union store n1 n2 | n1 == n2 = (n1, store)
                  | value n1 == value n2 = undefined
                  | value n1 < value n2 =
                    let ZNode v hi lo = n1
                        (hi', store') = union store (lookupById hi store) n2
                        hi'id = lookupByNode hi' store'
                        n1' = ZNode v hi'id lo
                    in (n1', insert n1' store)
                  | value n1 > value n2  = union store n2 n1
