module Test.AdventOfCode.Twenty21.Eight
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty21.Eight
import AdventOfCode.Twenty21.Eight.Segment (Segment(..), Signal, toString, signal)
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

