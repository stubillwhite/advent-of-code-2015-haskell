module AdventOfCode.Day01Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import AdventOfCode.Day01 (day01, finalFloor, stepsToBasement)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "finalfloor" $ do
    it "should return expected responses for example data" $ do
      finalFloor "(())" `shouldBe` 0
      finalFloor "()()" `shouldBe` 0
      finalFloor "(((" `shouldBe` 3
      finalFloor "(()(()(" `shouldBe` 3
      finalFloor "))(((((" `shouldBe` 3
      finalFloor "())" `shouldBe` -1
      finalFloor "))(" `shouldBe` -1
      finalFloor ")))" `shouldBe` -3
      finalFloor ")())())" `shouldBe` -3
  describe "stepsToBasement" $ do
    it "should return expected responses for example data" $ do
      stepsToBasement ")" `shouldBe` Just 1
      stepsToBasement "()())" `shouldBe` Just 5

