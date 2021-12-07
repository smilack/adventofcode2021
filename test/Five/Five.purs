module Test.AdventOfCode.Twenty21.Five
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty21.Five
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
  describe "Day Five" do
    it "parses input" $ quickCheck testParseLine
    describe "Part 1" do
      describe "consider only horizontal and vertical lines" isAxialSpec

testParseLine :: Int -> Int -> Int -> Int -> Result
testParseLine x1 y1 x2 y2 =
  ({ x1, y1, x2, y2 } === _) $ parseLine toString
  where
  toString = show x1 <> "," <> show y1 <> " -> " <> show x2 <> "," <> show y2

isAxialSpec ∷ Spec Unit
isAxialSpec = do
  it "accepts horizontals" do
    isAxial { x1: 962, y1: 644, x2: 93, y2: 644 } `shouldEqual` true
  it "accepts verticals" do
    isAxial { x1: 59, y1: 964, x2: 59, y2: 841 } `shouldEqual` true
  it "rejects diagonals (+ slope)" do
    isAxial { x1: 613, y1: 694, x2: 864, y2: 945 } `shouldEqual` false
  it "rejects diagonals (- slope)" do
    isAxial { x1: 650, y1: 290, x2: 580, y2: 360 } `shouldEqual` false