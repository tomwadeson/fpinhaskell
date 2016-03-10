module DataStructures.ListSpec where

import Prelude hiding (tail, head)

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
