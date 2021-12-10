module Test.AdventOfCode.Twenty21.Nine
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty21.Nine
import Data.Array (intersect, length, sort)
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
  describe "Day Nine" do
    it "reads input into arrays" do
      parseInput testInput1 `shouldEqual` testArrays1
    it "retrieves values from arrays" do
      get { x: 0, y: 0 } testArrays1 `shouldEqual` Just 1
      get { x: 1, y: 1 } testArrays1 `shouldEqual` Just 5
      get { x: 2, y: 1 } testArrays1 `shouldEqual` Just 6
      get { x: -2, y: 1 } testArrays1 `shouldEqual` Nothing
    it "finds coordinates adjacent to a point" do
      length (intersect (adjacentPoints { x: 0, y: 0 }) adjacentToOrigin)
        `shouldEqual`
          length adjacentToOrigin
    it "finds values adjacent to a point" do
      sort (adjacentValues { x: 0, y: 0 } testArrays1) `shouldEqual` [ 2, 4 ]
      sort (adjacentValues { x: 1, y: 1 } testArrays1) `shouldEqual` [ 2, 4, 6, 8 ]
      sort (adjacentValues { x: 2, y: 1 } testArrays1) `shouldEqual` [ 3, 5, 9 ]
    it "identifies low points" do
      isLowPoint { x: 0, y: 0 } testArrays1 `shouldEqual` true
      isLowPoint { x: 1, y: 1 } testArrays1 `shouldEqual` false
    it "adds low point risk levels" do
      assessRisk testArrays1 `shouldEqual` 2
      assessRisk (parseInput testInput2) `shouldEqual` 15
    it "makes cartesian products" do
      product (\a b -> [ a, b ]) [ 1, 2 ] [ 3, 4 ]
        `shouldEqual`
          [ [ 1, 3 ], [ 1, 4 ], [ 2, 3 ], [ 2, 4 ] ]
      product (\x y -> { x, y }) [ 0, 1 ] [ 0, 1 ]
        `shouldEqual`
          [ { x: 0, y: 0 }, { x: 0, y: 1 }, { x: 1, y: 0 }, { x: 1, y: 1 } ]

testInput1 :: String
testInput1 = "123\n456\n789"

testArrays1 :: Array (Array Int)
testArrays1 = [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ]

testInput2 :: String
testInput2 = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"

adjacentToOrigin :: Array Point
adjacentToOrigin = [ { x: -1, y: 0 }, { x: 1, y: 0 }, { x: 0, y: -1 }, { x: 0, y: 1 } ]