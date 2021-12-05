module AdventOfCode.Twenty21.Five
  ( main
  ) where

import Prelude
import Data.Array (concat)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

-- Part One: Given a list of lines (x1,y1 -> x2,y2), only considering the
--           vertical and horizontal ones, return the number of points where at
--           least two lines overlap.

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Five/input"
  let
    lines = parseLines input

  liftEffect do
    log "Part One:"
    log "Lines:"
    logShow lines

parseLines :: String -> Array Line
parseLines = map parseLine <<< split (Pattern "\n")

parseLine :: String -> Line
parseLine =
  mkLine
    <<< sequence
    <<< map fromString
    <<< concat
    <<< map (split (Pattern ","))
    <<< split (Pattern " -> ")

type Line = { x1 :: Int, y1 :: Int, x2 :: Int, y2 :: Int }

mkLine :: Maybe (Array Int) -> Line
mkLine (Just [ x1, y1, x2, y2 ]) = { x1, y1, x2, y2 }
mkLine _ = { x1: 0, y1: 0, x2: 0, y2: 0 }