module Test.AdventOfCode.Twenty21.Ten
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty21.Ten
import Data.List (List(..), (:))
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
  describe "Day Ten" do
    it "parses input" do
      parseInput testInput `shouldEqual` parsedTestInput

testInput :: String
testInput =
  """[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"""

parsedTestInput :: List (List Character)
parsedTestInput =
  (Bracket Open : Paren Open : Brace Open : Paren Open : Angle Open : Paren Open : Paren Open : Paren Close : Paren Close : Bracket Open : Bracket Close : Angle Close : Bracket Open : Bracket Open : Brace Open : Bracket Open : Bracket Close : Brace Open : Angle Open : Paren Open : Paren Close : Angle Open : Angle Close : Angle Close : Nil)
    : (Bracket Open : Paren Open : Paren Open : Paren Close : Bracket Open : Angle Open : Angle Close : Bracket Close : Paren Close : Bracket Close : Paren Open : Brace Open : Bracket Open : Angle Open : Brace Open : Angle Open : Angle Open : Bracket Open : Bracket Close : Angle Close : Angle Close : Paren Open : Nil)
    : (Brace Open : Paren Open : Bracket Open : Paren Open : Angle Open : Brace Open : Brace Close : Bracket Open : Angle Open : Angle Close : Bracket Open : Bracket Close : Brace Close : Angle Close : Brace Open : Bracket Open : Bracket Close : Brace Open : Bracket Open : Paren Open : Angle Open : Paren Open : Paren Close : Angle Close : Nil)
    : (Paren Open : Paren Open : Paren Open : Paren Open : Brace Open : Angle Open : Angle Close : Brace Close : Angle Open : Brace Open : Angle Open : Brace Open : Angle Open : Angle Close : Brace Close : Brace Open : Bracket Open : Bracket Close : Brace Open : Bracket Open : Bracket Close : Brace Open : Brace Close : Nil)
    : (Bracket Open : Bracket Open : Angle Open : Bracket Open : Paren Open : Bracket Open : Bracket Close : Paren Close : Paren Close : Angle Open : Paren Open : Bracket Open : Bracket Open : Brace Open : Brace Close : Bracket Open : Bracket Open : Paren Open : Paren Close : Bracket Close : Bracket Close : Bracket Close : Nil)
    : (Bracket Open : Brace Open : Bracket Open : Brace Open : Paren Open : Brace Open : Brace Close : Bracket Close : Brace Open : Brace Close : Brace Close : Paren Open : Bracket Open : Brace Open : Bracket Open : Brace Open : Brace Open : Brace Open : Brace Close : Brace Close : Paren Open : Bracket Open : Bracket Close : Nil)
    : (Brace Open : Angle Open : Bracket Open : Bracket Open : Bracket Close : Bracket Close : Angle Close : Brace Close : Angle Open : Brace Open : Bracket Open : Brace Open : Bracket Open : Brace Open : Bracket Open : Bracket Close : Brace Open : Paren Open : Paren Close : Bracket Open : Bracket Open : Bracket Open : Bracket Close : Nil)
    : (Bracket Open : Angle Open : Paren Open : Angle Open : Paren Open : Angle Open : Paren Open : Angle Open : Brace Open : Brace Close : Paren Close : Paren Close : Angle Close : Angle Open : Paren Open : Bracket Open : Bracket Close : Paren Open : Bracket Open : Bracket Close : Paren Open : Paren Close : Nil)
    : (Angle Open : Brace Open : Paren Open : Bracket Open : Paren Open : Bracket Open : Bracket Open : Paren Open : Angle Open : Angle Close : Paren Open : Paren Close : Paren Close : Brace Open : Brace Close : Bracket Close : Angle Close : Paren Open : Angle Open : Angle Open : Brace Open : Brace Open : Nil)
    : (Angle Open : Brace Open : Paren Open : Bracket Open : Brace Open : Brace Open : Brace Close : Brace Close : Bracket Open : Angle Open : Bracket Open : Bracket Open : Bracket Open : Angle Open : Angle Close : Brace Open : Brace Close : Bracket Close : Bracket Close : Bracket Close : Angle Close : Bracket Open : Bracket Close : Bracket Close : Nil)
    : Nil
