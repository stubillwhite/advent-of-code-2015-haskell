module AdventOfCode.Day02Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import AdventOfCode.Day02
  ( Parcel(..)
  , day02
  , paperRequiredToWrap
  , totalSquareFeetToWrap
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "paperRequiredToWrap" $ do
    it "should return expected responses for example data" $ do
      paperRequiredToWrap (Parcel 2 3 4) `shouldBe` 58
      paperRequiredToWrap (Parcel 1 1 10) `shouldBe` 43
  describe "totalSquareFeetToWrap" $ do
    it "should return the sum of paper required to wrap all parcels" $ do
      totalSquareFeetToWrap [(Parcel 2 3 4), (Parcel 1 1 10)] `shouldBe` 101
      
