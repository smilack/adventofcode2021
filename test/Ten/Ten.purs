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
  (Open Bracket : Open Paren : Open Brace : Open Paren : Open Angle : Open Paren : Open Paren : Close Paren : Close Paren : Open Bracket : Close Bracket : Close Angle : Open Bracket : Open Bracket : Open Brace : Open Bracket : Close Bracket : Open Brace : Open Angle : Open Paren : Close Paren : Open Angle : Close Angle : Close Angle : Nil)
    : (Open Bracket : Open Paren : Open Paren : Close Paren : Open Bracket : Open Angle : Close Angle : Close Bracket : Close Paren : Close Bracket : Open Paren : Open Brace : Open Bracket : Open Angle : Open Brace : Open Angle : Open Angle : Open Bracket : Close Bracket : Close Angle : Close Angle : Open Paren : Nil)
    : (Open Brace : Open Paren : Open Bracket : Open Paren : Open Angle : Open Brace : Close Brace : Open Bracket : Open Angle : Close Angle : Open Bracket : Close Bracket : Close Brace : Close Angle : Open Brace : Open Bracket : Close Bracket : Open Brace : Open Bracket : Open Paren : Open Angle : Open Paren : Close Paren : Close Angle : Nil)
    : (Open Paren : Open Paren : Open Paren : Open Paren : Open Brace : Open Angle : Close Angle : Close Brace : Open Angle : Open Brace : Open Angle : Open Brace : Open Angle : Close Angle : Close Brace : Open Brace : Open Bracket : Close Bracket : Open Brace : Open Bracket : Close Bracket : Open Brace : Close Brace : Nil)
    : (Open Bracket : Open Bracket : Open Angle : Open Bracket : Open Paren : Open Bracket : Close Bracket : Close Paren : Close Paren : Open Angle : Open Paren : Open Bracket : Open Bracket : Open Brace : Close Brace : Open Bracket : Open Bracket : Open Paren : Close Paren : Close Bracket : Close Bracket : Close Bracket : Nil)
    : (Open Bracket : Open Brace : Open Bracket : Open Brace : Open Paren : Open Brace : Close Brace : Close Bracket : Open Brace : Close Brace : Close Brace : Open Paren : Open Bracket : Open Brace : Open Bracket : Open Brace : Open Brace : Open Brace : Close Brace : Close Brace : Open Paren : Open Bracket : Close Bracket : Nil)
    : (Open Brace : Open Angle : Open Bracket : Open Bracket : Close Bracket : Close Bracket : Close Angle : Close Brace : Open Angle : Open Brace : Open Bracket : Open Brace : Open Bracket : Open Brace : Open Bracket : Close Bracket : Open Brace : Open Paren : Close Paren : Open Bracket : Open Bracket : Open Bracket : Close Bracket : Nil)
    : (Open Bracket : Open Angle : Open Paren : Open Angle : Open Paren : Open Angle : Open Paren : Open Angle : Open Brace : Close Brace : Close Paren : Close Paren : Close Angle : Open Angle : Open Paren : Open Bracket : Close Bracket : Open Paren : Open Bracket : Close Bracket : Open Paren : Close Paren : Nil)
    : (Open Angle : Open Brace : Open Paren : Open Bracket : Open Paren : Open Bracket : Open Bracket : Open Paren : Open Angle : Close Angle : Open Paren : Close Paren : Close Paren : Open Brace : Close Brace : Close Bracket : Close Angle : Open Paren : Open Angle : Open Angle : Open Brace : Open Brace : Nil)
    : (Open Angle : Open Brace : Open Paren : Open Bracket : Open Brace : Open Brace : Close Brace : Close Brace : Open Bracket : Open Angle : Open Bracket : Open Bracket : Open Bracket : Open Angle : Close Angle : Open Brace : Close Brace : Close Bracket : Close Bracket : Close Bracket : Close Angle : Open Bracket : Close Bracket : Close Bracket : Nil)
    : Nil
