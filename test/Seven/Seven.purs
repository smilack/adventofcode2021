module Test.AdventOfCode.Twenty21.Seven
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty21.Seven
import Data.Array.NonEmpty (NonEmptyArray, singleton, appendArray)
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
  describe "Day Seven" do
    describe "Input" do
      it "parses csv" do
        input `shouldEqual` inputCheck
      it "calculates fuel use" do
        cumulativeDistanceTo 2 input `shouldEqual` 37
        cumulativeDistanceTo 1 input `shouldEqual` 41
        cumulativeDistanceTo 3 input `shouldEqual` 39
        cumulativeDistanceTo 10 input `shouldEqual` 71
      it "finds minimum fuel position" do
        bestPosition input `shouldEqual` 2
      it "finds fuel use of best position" do
        solvePart1 strInput `shouldEqual` 37

strInput :: String
strInput = "16,1,2,0,4,2,7,1,2,14"

input :: NonEmptyArray Int
input = parseInput strInput

inputCheck :: NonEmptyArray Int
inputCheck = appendArray (singleton 16) [ 1, 2, 0, 4, 2, 7, 1, 2, 14 ]

