module Test.AdventOfCode.Twenty21.Five
  ( main
  ) where

import Prelude (Unit, discard, map, show, ($), (<>), (==), (||))
import AdventOfCode.Twenty21.Five (AxialLine(..), Line, enumerate, parseLine, toAxial)
import Data.List (length, group)
import Data.Maybe (isJust)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.QuickCheck ((===), Result)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Day Five" do
    it "parses input" $ quickCheck testParseLine
    describe "Part 1" do
      describe "axial checker" isAxialSpec
      it "processes axial lines" $ quickCheck testToAxial
      describe "enumerate" enumerateSpec

testParseLine :: Int -> Int -> Int -> Int -> Result
testParseLine x1 y1 x2 y2 =
  ({ x1, y1, x2, y2 } === _) $ parseLine toString
  where
  toString = show x1 <> "," <> show y1 <> " -> " <> show x2 <> "," <> show y2

testToAxial :: Line -> Result
testToAxial line = isAxial line === isJust (toAxial line)

isAxial :: Line -> Boolean
isAxial { x1, y1, x2, y2 } = x1 == x2 || y1 == y2

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

enumerateSpec :: Spec Unit
enumerateSpec = do
  it "all hori points have same y" do
    length (group $ map (_.y) $ enumerate $ Hori { x1: 962, y: 644, x2: 93 }) `shouldEqual` 1
  it "all verti points have same x" do
    length (group $ map (_.x) $ enumerate $ Verti { y1: 964, x: 59, y2: 841 }) `shouldEqual` 1