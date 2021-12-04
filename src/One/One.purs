module AdventOfCode.Twenty21.One
  ( main
  ) where

import Prelude
import Data.Int (fromString)
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

-- This report indicates that, scanning outward from the submarine, the sonar sweep found depths of 199, 200, 208, 210, and so on.
-- 
-- The first order of business is to figure out how quickly the depth increases, just so you know what you're dealing with - you never know if the keys will get carried into deeper water by an ocean current or a fish or something.
-- 
-- To do this, count the number of times a depth measurement increases from the previous measurement. (There is no measurement before the first measurement.) In the example above, the changes are as follows:
-- 
-- 199 (N/A - no previous measurement)
-- 200 (increased)
-- 208 (increased)
-- 210 (increased)
-- 200 (decreased)
-- 207 (increased)
-- 240 (increased)
-- 269 (increased)
-- 260 (decreased)
-- 263 (increased)
-- 
-- In this example, there are 7 measurements that are larger than the previous measurement.
-- 
-- How many measurements are larger than the previous measurement?

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/One/input"
  let
    measurements = parseIntArray input
    increases = countIncreases measurements
  liftEffect do
    log "Day one answer:"
    logShow increases

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