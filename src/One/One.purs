module AdventOfCode.Twenty21.One
  ( main
  ) where

import Prelude
import Data.Int (fromString)
import Data.Foldable (foldl)
import Data.List (List(..), fromFoldable, toUnfoldable, (:))
import Data.Maybe (fromMaybe)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

-- Part One: Given a list of integers, return the number of times a number is
--           larger than the preceding number

-- Part Two: Given a list of integers, return the number of times the sum of
--           three consecutive numbers is larger than the sum of the previous
--           three consecutive numbers (with overlap, i.e. if the list is [1, 2,
--           3, 4, 5], then compare (1+2+3), (2+3+4), (3+4+5))

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/One/input"
  let
    measurements = parseIntArray input
    increases = countIncreases measurements
    sums = slidingSums measurements
    slidingIncreases = countIncreases sums
  liftEffect do
    log "Number of increases:"
    logShow increases
    log "Number of increases (sliding sum):"
    logShow slidingIncreases

parseIntArray :: String -> Array Int
parseIntArray =
  map (fromMaybe 0 <<< fromString)
    <<< split (Pattern "\n")

countIncreases :: Array Int -> Int
countIncreases =
  _.increases
    <<< foldl f { last: -1, increases: -1 }
  where
  f :: Acc -> Int -> Acc
  f { last, increases } num
    | num > last = { last: num, increases: increases + 1 }
    | otherwise = { last: num, increases }

type Acc = { last :: Int, increases :: Int }

slidingSums :: Array Int -> Array Int
slidingSums = toUnfoldable <<< go <<< fromFoldable
  where
  go :: List Int -> List Int
  go (a : b : c : xs) = (a + b + c) : go (b : c : xs)
  go _ = Nil