module DataStructures.TreeSpec where

import Prelude hiding (maximum, map)

import Test.Hspec
import DataStructures.Tree

spec :: Spec
spec = do
  describe "size" $ do
    it "gives the size of a tree in terms of the count of its nodes" $ do
      let tree = Branch (Leaf 10) (Leaf 20)
      size tree `shouldBe` 3
      size' tree `shouldBe` 3
  describe "maximum" $ do
    it "finds the maximum value contained in a Tree Int" $ do
      let tree = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 5))
      maximum tree `shouldBe` 5
      maximum' tree `shouldBe` 5
  describe "depth" $ do
    it "returns the maximum path length from the root of a tree to the deepest leaf" $ do
      let tree = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 5))
      depth tree `shouldBe` 2
      depth' tree `shouldBe` 2
  describe "map" $ do
    it "applies a function to each leaf in a tree, preserving its structure" $ do
      let tree = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 5))
      map (*2) tree `shouldBe` Branch (Leaf 2) (Branch (Leaf 4) (Leaf 10))
      map' (*2) tree `shouldBe` Branch (Leaf 2) (Branch (Leaf 4) (Leaf 10))
