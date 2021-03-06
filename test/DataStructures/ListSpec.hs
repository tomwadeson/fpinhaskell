module DataStructures.ListSpec where

import Prelude hiding ( tail
                      , head
                      , drop
                      , dropWhile
                      , init
                      , sum
                      , product
                      , length
                      , reverse
                      , concat
                      , map
                      , filter 
                      , zipWith 
                      , any )

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
    it "summarises a list (right associative)" $ do
      let list = fromList [1..3]
      foldRight (+) 0 list `shouldBe` 6
      foldRight' (+) 0 list `shouldBe` 6
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
  describe "length" $ do
    it "computes the length of a list" $ do
      let list = fromList [1..5]
      length list `shouldBe` 5
      length' list `shouldBe` 5
  describe "foldLeft" $ do
    it "summarises a list (left associative)" $ do
      let list = fromList [1..3]
      foldLeft (+) 0 list `shouldBe` 6
  describe "reverse" $ do
    it "reverses a list" $ do
      let list = fromList [1..5]
      reverse list `shouldBe` Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil))))
  describe "append" $ do
    it "concatenates two lists" $ do
      let list1 = fromList [3..5]
      let list2 = fromList [1..2]
      append list1 list2 `shouldBe` Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))
  describe "concat" $ do
    it "concats/flattens a list of lists into a single list" $ do
      let list1 = fromList [1..2]
      let list2 = fromList [3..4]
      let list3 = fromList [5..7]
      let list = fromList [list1, list2, list3]
      concat list `shouldBe` Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 Nil))))))
  describe "addOneToAll" $ do
    it "adds 1 to every number in a list" $ do
      let list = fromList [1..5]
      addOneToAll list `shouldBe` fromList [2..6]
  describe "dtos" $ do
    it "converts a list of Doubles to a list of Strings" $ do
      let list = fromList [0.25, 0.5, 0.75]
      dtos list `shouldBe` fromList ["0.25", "0.5", "0.75"]
  describe "map" $ do
    it "applies the supplied function to each element contained in a list" $ do
      let list = fromList [1..5]
      map (*100) list `shouldBe` fromList [100, 200, 300, 400, 500]
  describe "filter" $ do
    it "filters elements from a list according to a given predicate" $ do
      let list = fromList [1..5]
      filter (<4) list `shouldBe` fromList [1..3]
      filter' (<4) list `shouldBe` fromList [1..3]
  describe "flatMap" $ do
    it "applies a function (which returns a map) to all elements and flattens the resulting list" $ do
      let list = fromList [1..3]
      flatMap (\x -> Cons x (Cons x Nil)) list `shouldBe` fromList [1,1,2,2,3,3]
  describe "addElems" $ do
    it "adds corresponding elements from two lists of numbers" $ do
      let list1 = fromList [1..3]
      let list2 = fromList [100, 200, 300]
      addElems list1 list2 `shouldBe` fromList [101, 202, 303]
  describe "zipWith" $ do
    it "applies the supplied function in a pairwise manner to elements in two lists" $ do
      let list1 = fromList [1..5]
      let list2 = fromList $ repeat 5 
      zipWith (-) list1 list2 `shouldBe` fromList [(-4), (-3), (-2), (-1), 0]
  describe "hasSubsequence" $ do
    it "determines whether a list contains another list" $ do
      let list = fromList "Hello, my name is Tom"
      (hasSubsequence list $ fromList "Hello") `shouldBe` True
      (hasSubsequence list $ fromList "name") `shouldBe` True
      (hasSubsequence list $ fromList "Tom") `shouldBe` True
  describe "tails" $ do
    it "produces a list of recurisvely calling tail on the input list" $ do
      let listTails = toList . map toList . tails $ fromList [1..3]
      listTails `shouldBe` [[1,2,3], [2,3], [3]]
  describe "isPrefixOf" $ do
    it "determines whether a list begins with a second list" $ do
      let list = fromList "Hello, World"
      (fromList "Hello" `isPrefixOf` list) `shouldBe` True
      (fromList "'ello" `isPrefixOf` list) `shouldBe` False
