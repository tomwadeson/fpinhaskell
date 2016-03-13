module DataStructures.TreeSpec where

import Test.Hspec
import DataStructures.Tree

spec :: Spec
spec = do
  describe "size" $ do
    it "gives the size of a tree in terms of the count of its nodes" $ do
      let tree = Branch (Leaf 10) (Leaf 20)
      size tree `shouldBe` 3
