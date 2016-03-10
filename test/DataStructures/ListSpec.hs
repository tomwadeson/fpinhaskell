module DataStructures.ListSpec where

import Prelude hiding (tail)

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
