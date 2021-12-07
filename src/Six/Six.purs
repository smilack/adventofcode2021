module AdventOfCode.Twenty21.Six where

import Prelude
import Data.Int (fromString)
import Data.List (List(..), (:), fromFoldable, catMaybes)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Six/input"
  liftEffect $ log input

parseAges :: String -> List Int
parseAges =
  catMaybes
    <<< map fromString
    <<< fromFoldable
    <<< split (Pattern ",")