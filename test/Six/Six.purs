module Test.AdventOfCode.Twenty21.Six
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty21.Six
import Data.List (List(..), (:))
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
  describe "Day Six" do
    it "parses input" do
      parseAges "1,2,5,2,6,8,3,0,1,4" `shouldEqual` (1 : 2 : 5 : 2 : 6 : 8 : 3 : 0 : 1 : 4 : Nil)
    pending "other stuff"

