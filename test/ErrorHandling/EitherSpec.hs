module ErrorHandling.EitherSpec where

import Prelude hiding (Either(..), map)

import Test.Hspec
import ErrorHandling.Either

spec :: Spec
spec = do
  describe "map" $ do
    context "Left" $ do 
      it "does nothing" $ do
        map (+1) (Left "Foo" :: Either String Int) `shouldBe` Left "Foo"
    context "Right" $ do
      it "applies a function to the Right value" $ do
        map (+1) (Right 1 :: Either String Int) `shouldBe` Right 2
  describe "flatMap" $ do
    context "Left" $ do
      it "does nothing" $ do
        map (+1) (Left "Foo" :: Either String Int) `shouldBe` Left "Foo"
    context "Right" $ do
      it "applies a function that returns an Either value and flattens the result" $ do
        let f x = if x == 1 then Right "One" else Left "Error"
        flatMap f (Right 1 :: Either String Int) `shouldBe` Right "One"
        flatMap f (Left "" :: Either String Int) `shouldBe` Left ""
  describe "orElse" $ do
    context "Left" $ do
      it "returns the second Either" $ do
        orElse (Left "" :: Either String Int) (Right 0 :: Either String Int) `shouldBe` Right 0
    context "Right" $ do
      it "returns the first Either" $ do
        orElse (Right 0 :: Either String Int) (Left "" :: Either String Int) `shouldBe` Right 0
  describe "map2" $ do
    it "lifts a function of two parameters into the Either context" $ do
      let e1 = (Right 1 :: Either String Int)
      let e2 = (Right 2 :: Either String Int)
      map2 (+) e1 e2 `shouldBe` Right 3
      map2 (+) (Left "" :: Either String Int) e1 `shouldBe` Left ""
      map2 (+) e1 (Left "" :: Either String Int) `shouldBe` Left ""
