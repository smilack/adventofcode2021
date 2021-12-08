module Test.AdventOfCode.Twenty21.Eight
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty21.Eight
import AdventOfCode.Twenty21.Eight.Segment (Segment(..), Signal, toString, fromString, signal)
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
  describe "Day Eight" do
    describe "Segments" do
      it "prints a signal as a lowercase alphabetical string" do
        toString (signal [ A, B, C, D, E, F, G ]) `shouldEqual` "abcdefg"
        toString (signal [ A, A, A, A, A, A ]) `shouldEqual` "a"
        toString (signal [ F, E, D ]) `shouldEqual` "def"
      it "reads a signal from a string" do
        fromString "acedgfb" `shouldEqual` signal [ A, C, E, D, G, F, B ]
        fromString "gcdfa" `shouldEqual` signal [ G, C, D, F, A ]
        fromString "cagedb" `shouldEqual` signal [ C, A, G, E, D, B ]
        fromString "ab" `shouldEqual` signal [ A, B ]
      it "parses a line of input" do
        parseInput testInput `shouldEqual` [ testParsedInput ]
      it "parses two lines of input" do
        parseInput (testInput <> "\n" <> testInput) `shouldEqual` [ testParsedInput, testParsedInput ]

testInput :: String
testInput = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

testParsedInput :: Reading
testParsedInput =
  { digits:
      [ signal [ A, C, E, D, G, F, B ]
      , signal [ C, D, F, B, E ]
      , signal [ G, C, D, F, A ]
      , signal [ F, B, C, A, D ]
      , signal [ D, A, B ]
      , signal [ C, E, F, A, B, D ]
      , signal [ C, D, F, G, E, B ]
      , signal [ E, A, F, B ]
      , signal [ C, A, G, E, D, B ]
      , signal [ A, B ]
      ]
  , output:
      [ signal [ C, D, F, E, B ]
      , signal [ F, C, A, D, B ]
      , signal [ C, D, F, E, B ]
      , signal [ C, D, B, A, F ]
      ]
  }