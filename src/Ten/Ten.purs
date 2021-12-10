module AdventOfCode.Twenty21.Ten where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:), fromFoldable, catMaybes)
import Data.Maybe (Maybe(..))
import Data.String.Utils (lines, toCharArray)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Ten/input"
  liftEffect $ log input

parseInput :: String -> List (List Character)
parseInput =
  map (catMaybes <<< map readCharacter)
    <<< map (fromFoldable <<< toCharArray)
    <<< fromFoldable
    <<< lines

-- checkLine :: List Character -> LineStatus
-- checkLine line =
--   where
--   go (c : cs) =

data LineStatus
  = Corrupt Character
  | Incomplete (List Character)

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