module DataStructures.ListSpec where

import Prelude hiding (tail, head, drop, dropWhile, init, sum, product)

import Test.Hspec
import Control.Exception (evaluate)
import DataStructures.List

spec :: Spec
spec = do
  describe "fromList" $ do
    it "builds a List a from a [a]" $ do
      let list = fromList [1..5]
      list `shouldBe` Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))
  describe "tail" $ do
    context "empty list" $ do
      it "is undefined" $ do
        evaluate (tail Nil) `shouldThrow` anyErrorCall 
    context "non-empty list" $ do
      it "returns all but the first element" $ do
        let list = fromList [1..3]
        tail list `shouldBe` Cons 2 (Cons 3 Nil)
  describe "head" $ do
    context "empty list" $ do
      it "is undefined" $ do
        evaluate (head Nil) `shouldThrow` anyErrorCall
    context "non-empty list" $ do
      it "returns the first element in the list" $ do
        let list = fromList [1..5]
        head list `shouldBe` 1
  describe "setHead" $ do
    context "empty list" $ do
      it "is undefined" $ do
        evaluate (setHead Nil 1) `shouldThrow` anyErrorCall
    context "non-empty list" $ do
      it "returns the first element in the list" $ do
        let list = fromList [1..3]
        setHead list 100 `shouldBe` Cons 100 (Cons 2 (Cons 3 Nil))
  describe "drop" $ do
    context "empty list" $ do
      it "returns an empty list" $ do
        (drop (Nil :: List Int)  2) `shouldBe` Nil
    context "non-empty list" $ do
      it "removes the first n elements from the list" $ do
        let list = fromList [1..5]
        drop list 4 `shouldBe` Cons 5 Nil
  describe "dropWhile" $ do
    context "empty list" $ do
      it "returns an empty list" $ do
        (dropWhile Nil (<10)) `shouldBe` Nil
    context "non-empty list" $ do
      it "removes elements from the beginning of the list while the predicate holds true" $ do
        let list = fromList [1..5]
        dropWhile list (<=3) `shouldBe` Cons 4 (Cons 5 Nil)
  describe "init" $ do
    context "empty list" $ do
      it "is undefined" $ do
        evaluate (init Nil) `shouldThrow` anyErrorCall 
    context "non-empty list" $ do
      it "returns all but the final element" $ do
        let list = fromList [1..3]
        init list `shouldBe` Cons 1 (Cons 2 Nil) 
  describe "foldRight" $ do
    it "summarises a list" $ do
      let list = fromList [1..3]
      foldRight (+) 0 list `shouldBe` 6
  describe "sum and sum'" $ do
    it "compute the sum of a list of numbers" $ do
      let list = fromList [1..5]
      sum list `shouldBe` 15
      sum' list `shouldBe` 15
  describe "product and product'" $ do
    it "compute the product of a list of numbers" $ do
      let list = fromList [1..5]
      product list `shouldBe` 120
      product' list `shouldBe` 120
