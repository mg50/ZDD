{-# LANGUAGE BangPatterns #-}
module ZDD where
import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Control.Applicative ((<$>))
import Debug.Trace

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
union store n1 n2 = runState (go n1 n2) store
  where go Bottom node = return node
        go node Bottom = return node
        go Top Top = return Top
        go (ZNode v hiId loId) Top = do lo <- byId loId
                                        lo' <- go lo Top
                                        loId' <- byNode lo'
                                        let node = ZNode v hiId loId'
                                        modify (insert node)
                                        return node
        go Top n = go n Top
        go n1 n2 | n1 == n2 = return n1
        go n1 n2 | value n1 == value n2 = do let ZNode v1 hiId1 loId1 = n1
                                                 ZNode v2 hiId2 loId2 = n2
                                             lo1 <- byId loId1
                                             lo2 <- byId loId2
                                             hi1 <- byId hiId1
                                             hi2 <- byId hiId2

                                             hi' <- go hi1 hi2
                                             hiId' <- byNode hi'
                                             lo' <- go lo1 lo2
                                             loId' <- byNode lo'

                                             let node = ZNode v1 hiId' loId'
                                             modify (insert node)
                                             return node

        go n1 n2 | value n1 < value n2 = do let ZNode v hiId loId = n1
                                            lo <- byId loId
                                            lo' <- go lo n2
                                            loId' <- byNode lo'
                                            let node = ZNode v hiId loId'
                                            modify (insert node)
                                            return node
        go n1 n2 | otherwise = go n2 n1

        byId x = lookupById x <$> get
        byNode x = lookupByNode x <$> get


type ZDDM a = State (ZStore a)

runZDD :: (Ord a) => ZDDM a (ZNode a) -> (ZNode a, ZStore a)
runZDD m = runState m emptyZStore

allElems :: (Ord a) => [a] -> ZDDM a (ZNode a)
allElems xs = go xs'
  where xs' = L.sort $ L.nub xs
        go [] = return Top
        go (x:xs) = do node <- go xs
                       hiId <- lookupByNode node <$> get
                       let node' = ZNode x hiId 0
                       modify (insert node')
                       return node'

family :: (Ord a) => [[a]] -> ZDDM a (ZNode a)
family [] = return Bottom
family (x:xs) = do node <- family xs
                   node' <- allElems x
                   unionM node node'
  where unionM x y = do store <- get
                        let (n, store') = union store x y
                        put store'
                        return n
