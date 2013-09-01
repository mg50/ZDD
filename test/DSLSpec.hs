module DSLSpec where
import Test.Hspec
import Control.Monad
import Control.Monad.State
import ZDD
import DSL
import qualified Data.List as L
import qualified Data.Set as S

shouldMatch :: ZDDM Int (ZNode Int) -> [[Int]] -> IO ()
shouldMatch m fam = let (node, store) = runZDD m
                        fam' = toList store (lookupByNode node store)
                    in S.fromList (map L.nub fam) `shouldBe` S.fromList fam'

debug m = print $ runZDD m

spec = do
  describe "allElems" $ do
    it "constructs top from an empty list" $ do
      allElems [] `shouldMatch` [[]]
    it "constructs ZDD of elements from a list" $ do
      allElems [3, 4, 8] `shouldMatch` [[3, 4, 8]]
    it "eliminates duplicate nodes" $ do
      allElems [2, 3, 4, 2] `shouldMatch` [[2..4]]

  describe "union" $ do
    it "takes the union of boring cases" $ do
      union Top Top `shouldMatch` [[]]
      union Top Bottom `shouldMatch` [[]]
      union Bottom Top `shouldMatch` [[]]
      union Bottom Bottom `shouldMatch` []

    it "takes the union of nodes with bottom" $ do
      let node = allElems [1..6]
      (node >>= (Bottom `union`)) `shouldMatch` [[1..6]]
      (node >>= (`union` Bottom)) `shouldMatch` [[1..6]]

    it "takes the union of a node with top" $ do
      let node = allElems [1..6]
      (node >>= (`union` Top)) `shouldMatch` [[1..6], []]
      (node >>= (Top `union`)) `shouldMatch` [[1..6], []]

    it "takes the union of a simple node with itself" $ do
      let result = do x <- allElems [1..6]
                      y <- allElems [1..6]
                      union x y
      result `shouldMatch` [[1..6]]

    it "takes the union of two nodes with equal root" $ do
      let result = do x <- allElems [1..6]
                      y <- allElems [1..7]
                      union x y

      result `shouldMatch` [[1..6], [1..7]]

    it "takes the union of two nodes with different root" $ do
      let result = do x <- allElems [1..6]
                      y <- allElems [2..7]
                      union x y

      result `shouldMatch` [[1..6], [2..7]]

  describe "family" $ do
    it "converts a family into a ZDD" $ do
      let fam = [[1..3], [2..5], [2, 4], []]
      family fam `shouldMatch` fam

    it "constructs the same node" $ do
      let fam = [[1..3], [2..5], [2, 4], []]
          result = do f1 <- family fam
                      f2 <- family fam
                      return $ f1 == f2
      evalState result emptyZStore `shouldBe` True

  describe "intersection" $ do
    it "intersects trivial cases" $ do
      intersection Bottom Bottom `shouldMatch` []
      intersection Bottom Top `shouldMatch` []
      intersection Top Bottom `shouldMatch` []
      intersection Top Top `shouldMatch` [[]]


    it "intersects nodes with bottom" $ do
      let result1 = do node <- family [[1], [2..4], []]
                       intersection node Bottom
          result2 = do node <- family [[1], [2..4], []]
                       intersection Bottom node
      result1 `shouldMatch` []
      result2 `shouldMatch` []

    it "intersects nodes with top" $ do
      let result1 = do node <- family [[1], [2..4], []]
                       intersection node Top
          result2 = do node <- family [[1], [2..4], []]
                       intersection Top node
          result3 = do node <- family [[1], [2..4]]
                       intersection node Top
          result4 = do node <- family [[1], [2..4]]
                       intersection Top node
      result1 `shouldMatch` [[]]
      result2 `shouldMatch` [[]]
      result3 `shouldMatch` []
      result4 `shouldMatch` []

    it "takes the intersection of families" $ do
      let fam1 = [[1], [1, 2], [2..4], []]
          fam2 = [[1, 2], [4..8], [1]]
          result = do f1 <- family fam1
                      f2 <- family fam2
                      intersection f1 f2
      result `shouldMatch` [[1], [1, 2]]
