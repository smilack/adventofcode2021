module Test.AdventOfCode.Twenty21.Eleven
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty21.Eleven
import AdventOfCode.Twenty21.Eleven.Grid (Grid)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
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
  describe "Day Eleven" do
    it "Parses input" do
      map (foldMap show) parsedTestIn `shouldEqual` Just testInOneLine

parsedTestIn :: Maybe (Grid Octopus)
parsedTestIn = parseInput testIn

testIn :: String
testIn =
  """5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"""

testInOneLine :: String
testInOneLine =
  "5483143223274585471152645561736141336146635738547841675246452176841721688288113448468485545283751526"