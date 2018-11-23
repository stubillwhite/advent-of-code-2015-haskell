module AdventOfCode.Day03Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import AdventOfCode.Day03 (solutionPartOne)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solutionPartOne" $ do
    it "should return expected responses for example data" $ do
      (solutionPartOne ">") `shouldBe` Right 2
      (solutionPartOne "^>v<") `shouldBe` Right 4
      (solutionPartOne "^v^v^v^v^v") `shouldBe` Right 2

