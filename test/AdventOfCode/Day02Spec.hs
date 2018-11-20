module AdventOfCode.Day02Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import AdventOfCode.Day02
  ( Parcel(..)
  , day02
  , paperRequiredForParcel
  , totalPaperRequired
  , ribbonRequiredForParcel
  , totalRibbonRequired
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "paperRequiredForParcel" $ do
    it "should return expected responses for example data" $ do
      paperRequiredForParcel (Parcel 2 3 4) `shouldBe` 58
      paperRequiredForParcel (Parcel 1 1 10) `shouldBe` 43

  describe "totalPaperRequired" $ do
    it "should return the sum of paper required to wrap all parcels" $ do
      totalPaperRequired [(Parcel 2 3 4), (Parcel 1 1 10)] `shouldBe` 101
      
  describe "ribbonRequiredForParcel" $ do
    it "should return expected responses for example data" $ do
      ribbonRequiredForParcel (Parcel 2 3 4) `shouldBe` 34
      ribbonRequiredForParcel (Parcel 1 1 10) `shouldBe` 14

  describe "totalRibbonRequired" $ do
    it "should return the sum of ribbon required to wrap all parcels" $ do
      totalRibbonRequired [(Parcel 2 3 4), (Parcel 1 1 10)] `shouldBe` 48
