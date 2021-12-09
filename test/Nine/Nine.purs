module Test.AdventOfCode.Twenty21.Nine
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty21.Nine
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
  describe "Day Nine" do
    it "reads input into arrays" do
      inputToIntArrays testInput1
        `shouldEqual`
          [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ]

testInput1 :: String
testInput1 = "123\n456\n789"

testInput2 :: String
testInput2 = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"