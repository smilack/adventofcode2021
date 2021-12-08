module AdventOfCode.Twenty21.Seven where

import Prelude
import Data.Array (catMaybes)
import Data.Array.NonEmpty (NonEmptyArray, nub, sortWith, head, singleton, fromArray)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.Ord (abs)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

-- Part one: Given a list of horizontal positions, find the position that, if
--           all were shifted to that position, there would be the least total
--           change. Return the sum of how far each moved.

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Seven/input"
  liftEffect do
    log "Part 1:"
    log "Best fuel use:"
    logShow $ solvePart1 input

parseInput :: String -> NonEmptyArray Int
parseInput =
  fromMaybe (singleton 0)
    <<< fromArray
    <<< catMaybes
    <<< map fromString
    <<< split (Pattern ",")

cumulativeDistanceTo :: Int -> NonEmptyArray Int -> Int
cumulativeDistanceTo p = sum <<< map (abs <<< (_ - p))

bestPosition :: NonEmptyArray Int -> Int
bestPosition xs = head $ sortWith (\p -> cumulativeDistanceTo p xs) $ nub xs

solvePart1 :: String -> Int
solvePart1 i = cumulativeDistanceTo (bestPosition input) input
  where
  input = parseInput i