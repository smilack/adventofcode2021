module AdventOfCode.Twenty21.Nine where

import Prelude
import Data.Array (catMaybes)
import Data.Int (fromString)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (toCharArray, lines)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Nine/input"
  liftEffect $ log input

inputToIntArrays :: String -> Array (Array Int)
inputToIntArrays = map (catMaybes <<< map fromString <<< toCharArray) <<< lines