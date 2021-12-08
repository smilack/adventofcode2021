module AdventOfCode.Twenty21.Seven where

import Prelude
import Data.Array (catMaybes)
import Data.Array.NonEmpty (NonEmptyArray, sortWith, head, singleton, fromArray, range)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.Ord (abs)
import Data.Semigroup.Foldable (maximum, minimum)
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

-- Part two: Same except the cost function slope isn't constant (f(x) = x), it's
--           increasing:
--             f(x) = x + f(x - 1)
--             f(0) = 0
--           Alternatively: f(x) = n(n+1)/2
--                x
--              \¯¯¯
--      f(x) =   \    x
--               /
--              /___
--               i=1

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Seven/input"
  liftEffect do
    log "Part 1:"
    log "Best fuel use:"
    logShow $ solvePart1 input
    log "Part 2:"
    log "Best fuel use:"
    logShow $ solvePart2 input

parseInput :: String -> NonEmptyArray Int
parseInput =
  fromMaybe (singleton 0)
    <<< fromArray
    <<< catMaybes
    <<< map fromString
    <<< split (Pattern ",")

type CostFunction = Int -> Int -> Int

cumulativeDistanceTo :: CostFunction -> Int -> NonEmptyArray Int -> Int
cumulativeDistanceTo costFn p = sum <<< map (costFn p)

bestPosition :: CostFunction -> NonEmptyArray Int -> Int
bestPosition costFn xs =
  head $ sortWith (\p -> cumulativeDistanceTo costFn p xs) possibles
  where
  possibles = range (minimum xs) (maximum xs)

solvePart1 :: String -> Int
solvePart1 i =
  (cumulativeDistanceTo constantCost) (bestPosition constantCost input) input
  where
  input = parseInput i

constantCost :: CostFunction
constantCost x p = abs $ x - p

solvePart2 :: String -> Int
solvePart2 i =
  (cumulativeDistanceTo increasingCost) (bestPosition increasingCost input) input
  where
  input = parseInput i

increasingCost :: CostFunction
increasingCost x p = n * (n + 1) / 2
  where
  n = abs $ x - p