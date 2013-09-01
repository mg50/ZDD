module DSL where
import Control.Monad hiding (join)
import qualified Control.Monad as Monad
import qualified Data.Map as M
import Control.Monad.State
import Control.Applicative ((<$>))
import qualified Data.List as L
import ZDD

type ZDDM a = State (ZStore a)

runZDD :: (Ord a) => ZDDM a (ZNode a) -> (ZNode a, ZStore a)
runZDD m = runState m emptyZStore

by :: (Ord a) => ZNode a -> (ZNode a -> Int) -> ZDDM a (ZNode a)
by node sel = lookupById (sel node) <$> get

reduceM :: (Ord a) => ZNode a -> ZDDM a (ZNode a)
reduceM node = reduce node <$> get

getId :: (Ord a) => ZDDM a (ZNode a) -> ZDDM a Int
getId m = do node <- m
             lookupByNode node <$> get

yield :: (Ord a) => ZNode a -> ZDDM a (ZNode a)
yield n = do modify (insert n)
             return n

allElems :: (Ord a) => [a] -> ZDDM a (ZNode a)
allElems xs = go xs'
  where xs' = L.sort $ L.nub xs
        go [] = return Top
        go (x:[]) = yield $ ZNode x 1 0
        go (x:xs) = do hiId <- getId $ go xs
                       yield $ ZNode x hiId 0

withMemo :: (Ord a) => (ZNode a -> ZNode a -> Operation a)
                    -> ZNode a
                    -> ZNode a
                    -> ZDDM a (ZNode a)
                    -> ZDDM a (ZNode a)
withMemo ctor x y m = do cache <- opCache <$> get
                         case M.lookup (ctor x y) cache of
                           Just val -> return val
                           Nothing -> case M.lookup (ctor y x) cache of
                             Just val -> return val
                             Nothing -> m >>= yield

union :: (Ord a) => ZNode a -> ZNode a -> ZDDM a (ZNode a)
union Bottom node = return node
union node Bottom = return node
union Top Top = return Top
union Top node = do loId' <- getId $ node `by` loId >>= union Top
                    yield node{loId = loId'}
union node Top = union Top node
union n1 n2 = withMemo Union n1 n2 $ case () of
  _ | n1 == n2 -> return n1
    | value n1 == value n2 -> do
        loId' <- (n1 `by` loId) `u` (n2 `by` loId)
        hiId' <- (n1 `by` hiId) `u` (n2 `by` hiId)
        return n1{loId = loId', hiId = hiId'}
    | value n1 < value n2 -> do
        loId' <- (n1 `by` loId) `u` return n2
        return n1{loId = loId'}
    | otherwise -> union n2 n1
  where u x y = getId . Monad.join $ liftM2 union x y

family :: (Ord a) => [[a]] -> ZDDM a (ZNode a)
family [] = return Bottom
family (x:xs) = do node <- family xs
                   node' <- allElems x
                   union node node'

intersection :: (Ord a) => ZNode a -> ZNode a -> ZDDM a (ZNode a)
intersection Bottom _ = return Bottom
intersection _ Bottom = return Bottom
intersection Top Top = return Top
intersection Top node = node `by` loId >>= intersection Top
intersection node Top = intersection Top node
intersection n1 n2 = withMemo Intersection n1 n2 $ case () of
  _ | n1 == n2 -> return n1
    | value n1 == value n2 -> do
        loId' <- getId $ (n1 `by` loId) `int` (n2 `by` loId)
        hiId' <- getId $ (n1 `by` hiId) `int` (n2 `by` hiId)
        reduceM n1{hiId = hiId', loId = loId'}
    | value n1 < value n2 -> do
        lo <- n1 `by` loId
        intersection lo n2
    | otherwise -> intersection n2 n1
  where int x y = Monad.join $ liftM2 intersection x y

-- -- join :: (Ord a) => ZNode a -> ZNode a -> ZDDM a (ZNode a)
-- -- join Bottom node = return node
-- -- join node Bottom = return node
