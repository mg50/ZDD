module DSLSpec where
import Test.Hspec
import Control.Monad
import Control.Monad.State
import ZDD
import DSL
import qualified Data.List as L
import qualified Data.Set as S

hasSpan :: ZDDM Int (ZNode Int) -> [[Int]] -> IO ()
hasSpan m fam = let (node, store) = runZDD m
                    fam' = toList store (lookupByNode node store)
                in S.fromList (map L.nub fam) `shouldBe` S.fromList fam'

debug m = print $ runZDD m

spec = do
  describe "allElems" $ do
    it "constructs top from an empty list" $ do
      allElems [] `hasSpan` [[]]
    it "constructs ZDD of elements from a list" $ do
      allElems [3, 4, 8] `hasSpan` [[3, 4, 8]]
    it "eliminates duplicate nodes" $ do
      allElems [2, 3, 4, 2] `hasSpan` [[2..4]]

  describe "union" $ do
    it "takes the union of boring cases" $ do
      union Top Top `hasSpan` [[]]
      union Top Bottom `hasSpan` [[]]
      union Bottom Top `hasSpan` [[]]
      union Bottom Bottom `hasSpan` []

    it "takes the union of nodes with bottom" $ do
      let node = allElems [1..6]
      (node >>= (Bottom `union`)) `hasSpan` [[1..6]]
      (node >>= (`union` Bottom)) `hasSpan` [[1..6]]

    it "takes the union of a node with top" $ do
      let node = allElems [1..6]
      (node >>= (`union` Top)) `hasSpan` [[1..6], []]
      (node >>= (Top `union`)) `hasSpan` [[1..6], []]

    it "takes the union of a simple node with itself" $ do
      let result = do x <- allElems [1..6]
                      y <- allElems [1..6]
                      union x y
      result `hasSpan` [[1..6]]

    it "takes the union of two nodes with equal root" $ do
      let result = do x <- allElems [1..6]
                      y <- allElems [1..7]
                      union x y

      result `hasSpan` [[1..6], [1..7]]

    it "takes the union of two nodes with different root" $ do
      let result = do x <- allElems [1..6]
                      y <- allElems [2..7]
                      union x y

      result `hasSpan` [[1..6], [2..7]]

  describe "family" $ do
    it "converts a family into a ZDD" $ do
      let fam = [[1..3], [2..5], [2, 4], []]
      family fam `hasSpan` fam

    it "constructs the same node" $ do
      let fam = [[1..3], [2..5], [2, 4], []]
          result = do f1 <- family fam
                      f2 <- family fam
                      return $ f1 == f2
      evalState result emptyZStore `shouldBe` True

  describe "intersection" $ do
    it "intersects trivial cases" $ do
      intersection Bottom Bottom `hasSpan` []
      intersection Bottom Top `hasSpan` []
      intersection Top Bottom `hasSpan` []
      intersection Top Top `hasSpan` [[]]


    it "intersects nodes with bottom" $ do
      let result1 = do node <- family [[1], [2..4], []]
                       intersection node Bottom
          result2 = do node <- family [[1], [2..4], []]
                       intersection Bottom node
      result1 `hasSpan` []
      result2 `hasSpan` []

    it "intersects nodes with top" $ do
      let result1 = do node <- family [[1], [2..4], []]
                       intersection node Top
          result2 = do node <- family [[1], [2..4], []]
                       intersection Top node
          result3 = do node <- family [[1], [2..4]]
                       intersection node Top
          result4 = do node <- family [[1], [2..4]]
                       intersection Top node
      result1 `hasSpan` [[]]
      result2 `hasSpan` [[]]
      result3 `hasSpan` []
      result4 `hasSpan` []

    it "takes the intersection of families" $ do
      let fam1 = [[1], [1, 2], [2..4], []]
          fam2 = [[1, 2], [4..8], [1]]
          result = do f1 <- family fam1
                      f2 <- family fam2
                      intersection f1 f2
      result `hasSpan` [[1], [1, 2]]
