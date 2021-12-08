module Test.AdventOfCode.Twenty21.Eight
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty21.Eight (Key, Reading, collectData, countMatchingOutputDigits, decodeOutput, is1478, parseInput, solve, digitsToNum)
import AdventOfCode.Twenty21.Eight.Segment (Segment(..), toString, fromString, signal)
import Data.Map (fromFoldable)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
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
      it "recognizes 1s, 4s, 7s, 8s" do
        is1478 (signal [ A, C, E, D, G, F, B ]) `shouldEqual` true
        is1478 (signal [ C, D, F, B, E ]) `shouldEqual` false
        is1478 (signal [ G, C, D, F, A ]) `shouldEqual` false
        is1478 (signal [ F, B, C, A, D ]) `shouldEqual` false
        is1478 (signal [ D, A, B ]) `shouldEqual` true
        is1478 (signal [ C, E, F, A, B, D ]) `shouldEqual` false
        is1478 (signal [ C, D, F, G, E, B ]) `shouldEqual` false
        is1478 (signal [ E, A, F, B ]) `shouldEqual` true
        is1478 (signal [ C, A, G, E, D, B ]) `shouldEqual` false
        is1478 (signal [ A, B ]) `shouldEqual` true
      it "counts 1s, 4s, 7s, 8s, in a reading" do
        countMatchingOutputDigits is1478 [ testParsedInput ] `shouldEqual` 0
        countMatchingOutputDigits is1478 all1478Input `shouldEqual` 4
      it "solves a reading" do
        solve testParsedInput `shouldEqual` solvedTest
      it "decodes output" do
        decodeOutput testParsedInput `shouldEqual` 5353
      it "sums output" do
        collectData all1478Input `shouldEqual` 1178
        collectData [ testParsedInput ] `shouldEqual` 5353
      it "turns array into number" do
        digitsToNum [ 5, 3, 5, 3 ] `shouldEqual` 5353

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

all1478Input :: Array Reading
all1478Input = parseInput "bgcade acbedgf gbef cef cdbfa ef cbefd afgecd bgdce fbcgde | ef ef fce dgecfba"

solvedTest :: Key
solvedTest =
  fromFoldable
    [ signal [ A, C, E, D, G, F, B ] /\ 8
    , signal [ C, D, F, B, E ] /\ 5
    , signal [ G, C, D, F, A ] /\ 2
    , signal [ F, B, C, A, D ] /\ 3
    , signal [ D, A, B ] /\ 7
    , signal [ C, E, F, A, B, D ] /\ 9
    , signal [ C, D, F, G, E, B ] /\ 6
    , signal [ E, A, F, B ] /\ 4
    , signal [ C, A, G, E, D, B ] /\ 0
    , signal [ A, B ] /\ 1
    ]