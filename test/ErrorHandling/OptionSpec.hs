module ErrorHandling.OptionSpec where

import Prelude hiding (map, filter)

import ErrorHandling.Option
import Test.Hspec

spec :: Spec
spec = do
  describe "map" $ do
    it "should apply a function to a Some and not to a None" $ do
      let o1 = Some 42
      let o2 = (None :: Option Int)
      map (+2) o1 `shouldBe` Some 44
      map (+2) o2 `shouldBe` None
  describe "flatMap" $ do
    it "applies a function that returns an Option and flattens the result" $ do
      let o1 = Some "Tom"
      let o2 = Some "Lee"
      let o3 = None
      let getFavColour x = if x == "Tom" then None else Some "Blue"
      flatMap getFavColour o1 `shouldBe` None
      flatMap getFavColour o2 `shouldBe` Some "Blue"
      flatMap getFavColour o3 `shouldBe` None
  describe "getOrElse" $ do
    it "gets the contained value from an Optional if one is present, or a supplied default otherwise" $ do
      let o1 = Some 42
      let o2 = (None :: Option Int)
      getOrElse o1 100 `shouldBe` 42
      getOrElse o2 100 `shouldBe` 100
  describe "orElse" $ do
    it "return the first Option if present, or the second otherwise" $ do
      let o1 = Some 42
      let o2 = (None :: Option Int)
      orElse o1 o2 `shouldBe` o1
      orElse o2 o1 `shouldBe` o1
  describe "filter" $ do
    context "Some x" $ do
      it "returns Some x if the predicate holds and None otherwise" $ do
        let o1 = Some 30
        let o2 = Some 15
        filter (>18) o1 `shouldBe` Some 30
        filter (>18) o2 `shouldBe` None 
    context "None" $ do
      it "returns None" $ do
        filter (>18) None `shouldBe` None
