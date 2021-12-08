module AdventOfCode.Twenty21.Eight where

import Prelude
import AdventOfCode.Twenty21.Eight.Segment (Segment(..), Signal, toString, signal)
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
  input <- readTextFile UTF8 "./src/Eight/input"
  liftEffect do
    log input

splitLines :: String -> Array String
splitLines = split (Pattern "\n")

-- parseLine :: String -> 