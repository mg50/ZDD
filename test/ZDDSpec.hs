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

spec = do
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
      reduce (ZNode 5 0 2) store `shouldBe` node

    it "does not reduce a node whose hi exists and is not bottom" $ do
      reduce (ZNode 5 2 0) store `shouldBe` ZNode 5 2 0

    it "does not a reduce a node whose hi does not exist" $ do
      reduce (ZNode 5 3 0) store `shouldBe` ZNode 5 3 0

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
