{-# LANGUAGE BangPatterns #-}
module ZDD where
import Prelude hiding (lookup)
import qualified Data.Map as M
import Data.Maybe

data ZNode a = ZNode { value :: a
                     , hiId :: !Int
                     , loId :: !Int
                     }
               | Top
               | Bottom
               deriving (Eq, Show, Ord)

data ZRecord a = ZRecord a !Int !Int
               | TopRec
               | BotRec deriving (Eq, Ord, Show)

data Operation a = Union (ZNode a) (ZNode a)
                 | Intersection (ZNode a) (ZNode a)
                 deriving (Show, Eq, Ord)

data ZStore a = ZStore { counter :: !Int
                       , nodesById :: M.Map Int (ZNode a)
                       , idsByNode :: M.Map (ZNode a) Int
                       , opCache :: M.Map (Operation a) (ZNode a)
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
emptyZStore = ZStore 2 a b M.empty
  where a = M.fromList [(0, Bottom), (1, Top)]
        b = M.fromList [(Bottom, 0), (Top, 1)]

insert :: (Ord a) => ZNode a -> ZStore a -> ZStore a
insert zn zst@(ZStore ctr n i cache) = case M.lookup zn (idsByNode zst)  of
  Just _  -> zst
  Nothing -> ZStore ctr' n' i' cache
               where ctr' = ctr + 1
                     n' = M.insert ctr zn n
                     i' = M.insert zn ctr i

reduce :: (Ord a) => ZNode a -> ZStore a -> ZNode a
reduce zn zst = if hiId zn == 0
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
