module DSL where
import Control.Monad hiding (join)
import qualified Control.Monad as Monad
import qualified Data.Map as M
import Control.Monad.State hiding (join)
import Control.Applicative ((<$>), (<*>))
import qualified Data.List as L
import ZDD
import Debug.Trace

type ZDDM a = State (ZStore a)

runZDD :: (Ord a) => ZDDM a (ZNode a) -> (ZNode a, ZStore a)
runZDD m = runState m emptyZStore

by :: (Ord a) => ZNode a -> (ZNode a -> Int) -> ZDDM a (ZNode a)
by node sel = lookupById (sel node) <$> get

reduceM :: (Ord a) => ZNode a -> ZDDM a (ZNode a)
reduceM node = reduce node <$> get

yield :: (Ord a) => ZNode a -> ZDDM a (ZNode a)
yield n = do modify (insert n)
             return n

getId m = do node <- m
             lookupByNode node <$> get

makeNode :: (Ord a) => a -> ZNode a -> ZNode a -> ZDDM a (ZNode a)
makeNode val hi lo = do hiId <- getId (return hi)
                        loId <- getId (return lo)
                        yield $ ZNode val hiId loId

allElems :: (Ord a) => [a] -> ZDDM a (ZNode a)
allElems xs = go xs'
  where xs' = L.sort $ L.nub xs
        go [] = return Top
        go (x:[]) = yield $ ZNode x 1 0
        go (x:xs) = do hi <- go xs
                       makeNode x hi Bottom

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
                             Nothing -> do node <- m
                                           insertOpCache (ctor x y) node <$> get
                                           yield node

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
        lo' <- (n1 `by` loId) `u` (n2 `by` loId)
        hi' <- (n1 `by` hiId) `u` (n2 `by` hiId)
        makeNode (value n1) hi' lo'
    | value n1 < value n2 -> do
        loId' <- getId $ (n1 `by` loId) `u` return n2
        return n1{loId = loId'}
    | otherwise -> union n2 n1
  where u x y = Monad.join $ liftM2 union x y

unionM :: (Ord a) => ZDDM a (ZNode a) -> ZDDM a (ZNode a) -> ZDDM a (ZNode a)
unionM x y = Monad.join $ liftM2 union x y

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

join :: (Ord a, Show a) => ZNode a -> ZNode a -> ZDDM a (ZNode a)
join Bottom node = return Bottom
join node Bottom = return Bottom
join Top node = return node
join node Top = return node
join n1 n2
  | value n1 == value n2 = do
      lo' <- join' (n1 `by` loId) (n2 `by` loId)
      hi1 <- join' (n1 `by` hiId) (n2 `by` hiId)
      hi2 <- join' (n1 `by` hiId) (n2 `by` loId)
      hi3 <- join' (n1 `by` loId) (n2 `by` hiId)
      hi' <- foldM union hi1 [hi2, hi3]
      makeNode (value n1) hi' lo'
  | value n1 < value n2 = do
      hi' <- (n1 `by` hiId) >>= join n2
      lo' <- (n1 `by` loId) >>= join n2
      makeNode (value n1) hi' lo'
  | otherwise = join n2 n1
  where join' x y = Monad.join $ liftM2 join x y

meet :: (Ord a, Show a) => ZNode a -> ZNode a -> ZDDM a (ZNode a)
meet Bottom node = return Bottom
meet node Bottom = return Bottom
meet Top node = return Top
meet node Top = return Top
meet n1 n2
  | value n1 == value n2 = do
      hi' <- meet' (n1 `by` hiId) (n2 `by` hiId)
      lo1 <- meet' (n1 `by` loId) (n2 `by` loId)
      lo2 <- meet' (n1 `by` hiId) (n2 `by` loId)
      lo3 <- meet' (n1 `by` loId) (n2 `by` hiId)
      lo' <- foldM union lo1 [lo2, lo3]
      makeNode (value n1) hi' lo'
  | value n1 < value n2 = do
      hi' <- (n1 `by` hiId) >>= meet n2
      lo' <- (n1 `by` loId) >>= meet n2
      yield =<< (union hi' lo')
  | otherwise = meet n2 n1
  where meet' x y = Monad.join $ liftM2 meet x y
