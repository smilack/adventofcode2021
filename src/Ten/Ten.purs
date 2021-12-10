module AdventOfCode.Twenty21.Ten where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Foldable (sum)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:), fromFoldable, catMaybes, dropEnd, drop, sort, filter)
import Data.Maybe (Maybe(..))
import Data.String.Utils (lines, toCharArray)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Ten/input"
  liftEffect do
    log "Part 1:"
    log "Total syntax error score:"
    logShow $ solve1 input
    log "Part 2:"
    log "Median autocomplete score:"
    -- 29184309 too low
    logShow $ solve2 input

solve1 :: String -> Int
solve1 =
  sum
    <<< map (errorScore <<< checkLine)
    <<< parseInput

solve2 :: String -> Int
solve2 =
  median
    <<< sort
    <<< map (completionScore <<< missingCharacters)
    <<< filter isIncomplete
    <<< map checkLine
    <<< parseInput

parseInput :: String -> List (List Character)
parseInput =
  map (catMaybes <<< map readCharacter)
    <<< map (fromFoldable <<< toCharArray)
    <<< fromFoldable
    <<< lines

checkLine :: List Character -> LineStatus
checkLine line = go line Nil
  where
  go Nil Nil = Valid

  go Nil stack = Incomplete stack

  go (c : cs) Nil =
    if isOpen c then
      go cs (c : Nil)
    else
      Corrupt c

  go (c : cs) (s : stack) =
    if isOpen c then
      go cs (c : s : stack)
    else if c `closes` s then
      go cs stack
    else
      Corrupt c

errorScore :: LineStatus -> Int
errorScore = case _ of
  Valid -> 0
  Incomplete _ -> 0
  Corrupt character -> case character of
    Open _ -> 0
    Close Paren -> 3
    Close Bracket -> 57
    Close Brace -> 1197
    Close Angle -> 25137

missingCharacters :: LineStatus -> List Character
missingCharacters = case _ of
  Valid -> Nil
  Corrupt _ -> Nil
  Incomplete chars -> go chars
  where
  go Nil = Nil
  go (c : cs) = invert c : go cs

completionScore :: List Character -> Int
completionScore = go 0
  where
  go score Nil = score
  go score (c : cs) = go (5 * score + charScore c) cs

  charScore = case _ of
    Open _ -> 0
    Close Paren -> 1
    Close Bracket -> 2
    Close Brace -> 3
    Close Angle -> 4

median :: List Int -> Int
median = case _ of
  Nil -> 0
  (x : Nil) -> x
  xs -> median $ dropEnd 1 $ drop 1 xs

data LineStatus
  = Corrupt Character
  | Incomplete (List Character)
  | Valid

derive instance Generic LineStatus _

instance Eq LineStatus where
  eq = genericEq

instance Show LineStatus where
  show = genericShow

isIncomplete :: LineStatus -> Boolean
isIncomplete (Incomplete _) = true
isIncomplete _ = false

data CharType
  = Paren
  | Bracket
  | Brace
  | Angle

derive instance Generic CharType _

instance Eq CharType where
  eq = genericEq

data Character
  = Open CharType
  | Close CharType

derive instance Generic Character _

instance Eq Character where
  eq = genericEq

instance Show Character where
  show (Open Paren) = "("
  show (Close Paren) = ")"
  show (Open Bracket) = "["
  show (Close Bracket) = "]"
  show (Open Brace) = "{"
  show (Close Brace) = "}"
  show (Open Angle) = "<"
  show (Close Angle) = ">"

readCharacter :: String -> Maybe Character
readCharacter = case _ of
  "(" -> Just $ Open Paren
  ")" -> Just $ Close Paren
  "[" -> Just $ Open Bracket
  "]" -> Just $ Close Bracket
  "{" -> Just $ Open Brace
  "}" -> Just $ Close Brace
  "<" -> Just $ Open Angle
  ">" -> Just $ Close Angle
  _ -> Nothing

isOpen :: Character -> Boolean
isOpen (Open _) = true
isOpen (Close _) = false

closes :: Character -> Character -> Boolean
closes (Close c) (Open o) = c == o
closes _ _ = false

invert :: Character -> Character
invert (Open c) = Close c
invert (Close c) = Open c