module DataStructures.TreeSpec where

import Prelude hiding (maximum)

import Test.Hspec
import DataStructures.Tree

spec :: Spec
spec = do
  describe "size" $ do
    it "gives the size of a tree in terms of the count of its nodes" $ do
      let tree = Branch (Leaf 10) (Leaf 20)
      size tree `shouldBe` 3
  describe "maximum" $ do
    it "finds the maximum value contained in a Tree Int" $ do
      let tree = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 5))
      maximum tree `shouldBe` 5
