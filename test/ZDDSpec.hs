{-# LANGUAGE MultiParamTypeClasses #-}
module ZDDSpec where
import ZDD
import Test.Hspec
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad

withEmptyStore :: (Ord a) => (ZStore a -> ZStore a) -> ZStore a
withEmptyStore f = f emptyZStore

shouldContain store node = case M.lookup node (idsByNode store) of
                             Just _  -> True `shouldBe` True
                             Nothing -> True `shouldBe` False

shouldMatch :: (ZNode Int, ZStore Int) -> [[Int]] -> IO ()
shouldMatch (n1, store) l = let id = lookupByNode n1 store
                                l' = toList store id
                            in S.fromList (map L.nub l) `shouldBe` S.fromList l'

spec = describe "ZDD" $ do
  describe "insert" $ do
    it "adds a new node" $ do
      let node = ZNode 2 1 0
          store = insert node emptyZStore
      counter store `shouldBe` 3
      length (M.toList $ nodesById store) `shouldBe` 3
      length (M.toList $ idsByNode store) `shouldBe` 3

      M.lookup 2 (nodesById store) `shouldBe` Just node
      M.lookup node (idsByNode store) `shouldBe` Just 2

    it "ignores an existing node" $ do
      let store' = insert (ZNode 2 1 0) emptyZStore
          store  = insert (ZNode 2 1 0) store'

      counter store `shouldBe` 3
      length (M.toList $ nodesById store) `shouldBe` 3
      length (M.toList $ idsByNode store) `shouldBe` 3

    it "ignores top and bottom" $ do
      forM_ [Top, Bottom] $ \node -> do
        let store = insert Top emptyZStore :: ZStore Int
        counter store `shouldBe` 2
        length (M.toList $ nodesById store) `shouldBe` 2
        length (M.toList $ idsByNode store) `shouldBe` 2

  describe "reduce" $ do
    let node = ZNode 2 1 0
        store = insert node emptyZStore

    it "reduces a node whose hi points to bottom" $ do
      reduce store (ZNode 5 0 2) `shouldBe` node

    it "does not reduce a node whose hi exists and is not bottom" $ do
      reduce store (ZNode 5 2 0) `shouldBe` ZNode 5 2 0

    it "does not a reduce a node whose hi does not exist" $ do
      reduce store (ZNode 5 3 0) `shouldBe` ZNode 5 3 0

  describe "toList" $ do
    it "converts bottom to an empty list" $ do
      let store = emptyZStore :: ZStore Int
      toList store 0 `shouldBe` []

    it "converts top to a singleton list" $ do
      let store = emptyZStore :: ZStore Int
      toList store 1 `shouldBe` [[]]

    it "converts a simple ZDD to a list" $ do
      let node = ZNode 8 1 0
          store = insert node emptyZStore
      toList store 2 `shouldBe` [[8]]

    it "converts a slightly more complex ZDD to a list" $ do
      let node' = ZNode 9 1 1
          store' = insert node' emptyZStore
          node = ZNode 8 2 1
          store = insert node store'

      toList store 3 `shouldBe` [[8, 9], [8], []]

  describe "union'" $ do
    it "takes the union' of boring cases" $ do
      let node = ZNode 5 1 0
          store = emptyZStore :: ZStore Int

      union' Top Top store `shouldBe` (Top, store)
      union' Top Bottom store `shouldBe` (Top, store)

      union' Bottom Bottom store `shouldBe` (Bottom, store)
      union' Bottom Top store `shouldBe` (Top, store)

    it "takes the union' of nodes with bottom" $ do
      let node = ZNode 5 1 0
          store = emptyZStore :: ZStore Int

      union' node Bottom store `shouldBe` (node, store)
      union' Bottom node store `shouldBe` (node, store)

    it "takes the union' of a simple node with itself" $ do
      let node = ZNode 5 1 0
          store = insert node emptyZStore

      union' node node store `shouldBe` (node, store)

    it "takes the union' of a simple node with bottom" $ do
      let node = ZNode 5 1 0
          store = insert node emptyZStore

      union' node Bottom store `shouldBe` (node, store)
      union' Bottom node store `shouldBe` (node, store)

    it "takes the union' of a simple node with top" $ do
      let node = ZNode 5 1 0
          store = insert node emptyZStore
          (node', store') = union' node Top store

      node' `shouldBe` ZNode 5 1 1

    it "takes the union' of a simple node with top (in reverse)" $ do
      let node = ZNode 5 1 0
          store = insert node emptyZStore
          (node', store') = union' Top node store

      node' `shouldBe` ZNode 5 1 1
      store' `shouldContain` node'

    it "takes the union' of two equal nodes" $ do
      let node = ZNode 5 1 0
          store = insert node emptyZStore
          (node', store') = union' Top node store
          (node'', store'') = union' node' node' store'

      node' `shouldBe` node''
      store' `shouldBe` store''

    it "takes the union' of two nodes with unequal value" $ do
      let n1 = ZNode 5 1 0
          n2 = ZNode 6 1 0
          store = insert n1 emptyZStore
          store' = insert n2 store
          u = union' n1 n2 store'
      u `shouldMatch` [[5], [6]]

    it "takes the union' of two different nodes with equal value" $ do
      let n1 = ZNode 6 1 0
          n2 = ZNode 5 2 0
          n3 = ZNode 5 1 0
          store = insert n1 emptyZStore
          store' = insert n2 store
          store'' = insert n3 store'
          u = union' n2 n3 store''
      u `shouldMatch` [[5], [5, 6]]

  describe "allElems" $ do
    it "converts a list of elements to a ZDD" $ do
      let result = runZDD $ allElems [1..5]
      result `shouldMatch` [[1..5]]

    it "converts an empty list to top" $ do
      let result = runZDD $ allElems ([] :: [Int])
      result `shouldMatch` [[]]

  describe "family" $ do
    it "converts a family into a ZDD" $ do
      let fam = [[1..3], [2..5], [2, 4], []]
          result = runZDD $ family fam
      result `shouldMatch` fam

  describe "intersection" $ do
    it "intersects trivial cases" $ do
      runZDD (intersection Bottom Bottom) `shouldMatch` []
      runZDD (intersection Bottom Top) `shouldMatch` []
      runZDD (intersection Top Bottom) `shouldMatch` []
      runZDD (intersection Top Top) `shouldMatch` [[]]


    it "intersects nodes with bottom" $ do
      let result1 = runZDD $ do node <- family [[1], [2..4], []]
                                intersection node Bottom
          result2 = runZDD $ do node <- family [[1], [2..4], []]
                                intersection Bottom node
      result1 `shouldMatch` []
      result2 `shouldMatch` []

    it "intersects nodes with top" $ do
      let result1 = runZDD $ do node <- family [[1], [2..4], []]
                                intersection node Top
          result2 = runZDD $ do node <- family [[1], [2..4], []]
                                intersection Top node
          result3 = runZDD $ do node <- family [[1], [2..4]]
                                intersection node Top
          result4 = runZDD $ do node <- family [[1], [2..4]]
                                intersection Top node
      result1 `shouldMatch` [[]]
      result2 `shouldMatch` [[]]
      result3 `shouldMatch` []
      result4 `shouldMatch` []

    it "takes the intersection of families" $ do
      let fam1 = [[1], [1, 2], [2..4], []]
          fam2 = [[1, 2], [4..8], [1]]
          result = runZDD $ do f1 <- family fam1
                               f2 <- family fam2
                               intersection f1 f2

      result `shouldMatch` [[1], [1, 2]]
