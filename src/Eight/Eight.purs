module AdventOfCode.Twenty21.Eight where

import Prelude
import AdventOfCode.Twenty21.Eight.Segment (Segment(..), Signal, toString, fromString)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines, words)
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

type Reading = { digits :: Array Signal, output :: Array Signal }

parseInput :: String -> Array Reading
parseInput = map parseReading <<< map (split (Pattern " | ")) <<< lines
  where
  parseReading [ d, o ] = { digits: parseSignals d, output: parseSignals o }
  parseReading _ = { digits: [], output: [] }

  parseSignals = map fromString <<< words

-- # of segments per digit:
-- digit    0 1 2 3 4 5 6 7 8 9
-- segments 6 2 6 6 4 5 6 3 7 6