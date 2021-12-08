module Test.AdventOfCode.Twenty21.Seven
  ( main
  ) where

import Prelude (Unit, discard, ($))
import AdventOfCode.Twenty21.Seven (bestPosition, constantCost, cumulativeDistanceTo, increasingCost, parseInput, solvePart1, solvePart2)
import Data.Array.NonEmpty (NonEmptyArray, singleton, appendArray)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Day Seven" do
    it "parses csv" do
      input `shouldEqual` inputCheck
    describe "Part 1" do
      it "calculates fuel use" do
        cumulativeDistanceTo constantCost 2 input `shouldEqual` 37
        cumulativeDistanceTo constantCost 1 input `shouldEqual` 41
        cumulativeDistanceTo constantCost 3 input `shouldEqual` 39
        cumulativeDistanceTo constantCost 10 input `shouldEqual` 71
      it "finds minimum fuel position" do
        bestPosition constantCost input `shouldEqual` 2
      it "finds fuel use of best position" do
        solvePart1 strInput `shouldEqual` 37
    describe "Part 2" do
      it "calculates fuel use" do
        cumulativeDistanceTo increasingCost 5 input `shouldEqual` 168
        cumulativeDistanceTo increasingCost 2 input `shouldEqual` 206
      it "finds minimum fuel position" do
        bestPosition increasingCost input `shouldEqual` 5
      it "finds fuel use of best position" do
        solvePart2 strInput `shouldEqual` 168

strInput :: String
strInput = "16,1,2,0,4,2,7,1,2,14"

input :: NonEmptyArray Int
input = parseInput strInput

inputCheck :: NonEmptyArray Int
inputCheck = appendArray (singleton 16) [ 1, 2, 0, 4, 2, 7, 1, 2, 14 ]

