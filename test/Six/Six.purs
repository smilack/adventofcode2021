module Test.AdventOfCode.Twenty21.Six
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty21.Six
import Data.BigInt (fromInt)
import Data.List (List(..), (:))
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.QuickCheck ((===), Result)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Day Six" do
    it "parses input" do
      parseAges "1,2,5,2,6,8,3,0,1,4"
        `shouldEqual`
          (1 : 2 : 5 : 2 : 6 : 8 : 3 : 0 : 1 : 4 : Nil)
    it "populates FishTrack" do
      fillTrack (1 : 2 : 5 : 2 : 6 : 8 : 3 : 0 : 1 : 4 : Nil)
        `shouldEqual`
          FishTrack (map fromInt [ 1, 2, 2, 1, 1, 1, 1, 0, 1 ])
    it "simulates one day" do
      age (FishTrack (map fromInt [ 1, 2, 2, 1, 1, 1, 1, 0, 1 ]))
        `shouldEqual`
          FishTrack (map fromInt [ 2, 2, 1, 1, 1, 1, 1, 1, 1 ])
  sampleSpec

sampleSpec :: Spec Unit
sampleSpec =
  describe "Sample Data" do
    it "parses input" do
      parseAges "3,4,3,1,2"
        `shouldEqual`
          (3 : 4 : 3 : 1 : 2 : Nil)
    it "populates track" do
      (fillTrack $ parseAges "3,4,3,1,2")
        `shouldEqual`
          FishTrack (map fromInt [ 0, 1, 1, 2, 1, 0, 0, 0, 0 ])
    it "runs one day" do
      (age $ fillTrack $ parseAges "3,4,3,1,2")
        `shouldEqual`
          FishTrack (map fromInt [ 1, 1, 2, 1, 0, 0, 0, 0, 0 ])
    it "runs two days" do
      (ageBy 2 $ fillTrack $ parseAges "3,4,3,1,2")
        `shouldEqual`
          FishTrack (map fromInt [ 1, 2, 1, 0, 0, 0, 1, 0, 1 ])
    it "runs 18 days" do
      (ageBy 18 $ fillTrack $ parseAges "3,4,3,1,2")
        `shouldEqual`
          FishTrack (map fromInt [ 3, 5, 3, 2, 2, 1, 5, 1, 4 ])