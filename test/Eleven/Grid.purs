module Test.AdventOfCode.Twenty21.Eleven.Grid
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty21.Eleven.Grid
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
  describe "Grid" do
    pending "handles Maybes" --do
--fromMaybe Nothing `shouldEqual` empty
--fromMaybe (Just $ singleton 1) `shouldEqual` singleton 1