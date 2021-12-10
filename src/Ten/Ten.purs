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

data OpenClose
  = Open
  | Close

derive instance Generic OpenClose _

instance Eq OpenClose where
  eq = genericEq

data Character
  = Paren OpenClose
  | Bracket OpenClose
  | Brace OpenClose
  | Angle OpenClose

derive instance Generic Character _

instance Eq Character where
  eq = genericEq

instance Show Character where
  show (Paren Open) = "("
  show (Paren Close) = ")"
  show (Bracket Open) = "["
  show (Bracket Close) = "]"
  show (Brace Open) = "{"
  show (Brace Close) = "}"
  show (Angle Open) = "<"
  show (Angle Close) = ">"

readCharacter :: String -> Maybe Character
readCharacter = case _ of
  "(" -> Just $ Paren Open
  ")" -> Just $ Paren Close
  "[" -> Just $ Bracket Open
  "]" -> Just $ Bracket Close
  "{" -> Just $ Brace Open
  "}" -> Just $ Brace Close
  "<" -> Just $ Angle Open
  ">" -> Just $ Angle Close
  _ -> Nothing