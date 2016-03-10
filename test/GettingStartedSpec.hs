module GettingStartedSpec where

import Prelude hiding (curry, uncurry)

import Test.Hspec
import GettingStarted

spec :: Spec
spec = do
  describe "fib" $ do
    it "computes the nth number in the Fibonacci sequence" $ do
      fib 0 `shouldBe` 0
      fib 1 `shouldBe` 1
      fib 2 `shouldBe` 1
      fib 3 `shouldBe` 2
      fib 4 `shouldBe` 3
      fib 5 `shouldBe` 5
  describe "isSorted" $ do
    it "determines if a list is sorted accordning to a supplied function" $ do
      let as = [1,2,3,4,5]
      let bs = [5,4,3,2,1]
      as `shouldSatisfy` (`isSorted` (<))
      as `shouldNotSatisfy` (`isSorted` (>))
      bs `shouldSatisfy` (`isSorted` (>))
      bs `shouldNotSatisfy` (`isSorted` (<))
  describe "curry" $ do
    it "should curry a function that takes a 2-tuple parameter" $ do
      let fst' = curry fst
      fst' 10 20 `shouldBe` 10
  describe "uncurry" $ do
    it "should uncurry a function to explicitly take a 2-tuple parameter" $ do
      let add = uncurry (+)
      add (10, 20) `shouldBe` 30
  describe "compose" $ do
    it "should compose two functions, f and g, forming (f . g)" $ do
      let timesTwoPlusFive = compose (+5) (*2)
      timesTwoPlusFive 5 `shouldBe` 15
      timesTwoPlusFive 10 `shouldBe` 25
      timesTwoPlusFive (-2.5) `shouldBe` 0
