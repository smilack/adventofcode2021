module AdventOfCode.Twenty21.Six where

import Prelude
import Control.Parallel (parSequence)
import Data.Int (fromString)
import Data.List (List(..), (:), fromFoldable, catMaybes, length, take, drop, foldl, concat)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import PointFree ((<..))

-- Part One: Simulate fish lifecycle with rules:
--             * Age is represented as days until next spawn
--             * Decrement age each day
--             * A fish starting the day at age 0 is reset to 6 and a new fish
--               with age 8 appears
--           How many fish are there after 80 days?

-- Part Two: How many fish after 256 days?

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Six/input"
  let
    ages = parseAges input
    chunks = chunksOf 10 ages
  nFishChunks <- parSequence $ map (asyncSimulate 80) chunks
  nFish' <- bigSim ages
  -- will need to do partial calculations then re-chunk
  let
    nFish = foldl (+) 0 nFishChunks
  liftEffect do
    log "Day Six"
    log "Input:"
    logShow ages
    log "Part 1:"
    log "num fish"
    logShow nFish
    log "Part 2:"
    log "num fish"
    logShow nFish'

parseAges :: String -> List Int
parseAges =
  catMaybes
    <<< map fromString
    <<< fromFoldable
    <<< split (Pattern ",")

asyncSimulate :: Int -> List Int -> Aff Int
asyncSimulate = (pure <<< length) <.. simulate

bigSim :: List Int -> Aff Int
bigSim = go 256
  where
  go :: Int -> List Int -> Aff Int
  go 0 ages = pure $ length ages
  go n ages = do
    part <- partialSim 16 $ chunksOf 3000 ages
    liftEffect do
      log "n"
      logShow n
      log "length"
      logShow $ length $ concat part
    go (n - 16) $ concat part

  partialSim :: Int -> List (List Int) -> Aff (List (List Int))
  partialSim days chunks = parSequence $ map (sim days) chunks

  sim :: Int -> List Int -> Aff (List Int)
  sim = pure <.. simulate

simulate :: Int -> List Int -> List Int
simulate = go
  where
  go 0 l = l
  go n l = go (n - 1) (sim l)

  sim Nil = Nil
  sim (f : fs)
    | f == 0 = 6 : 8 : sim fs
    | otherwise = (f - 1) : sim fs

-- chunksOf -- From Haskell split library
-- https://github.com/byorgey/split
-- Copyright (c) 2008 Brent Yorgey, Louis Wasserman
-- https://github.com/byorgey/split/blob/master/LICENSE
chunksOf :: forall e. Int -> List e -> List (List e)
chunksOf i ls = map (take i) (build (splitter ls))
  where
  build :: forall a. ((a -> List a -> List a) -> List a -> List a) -> List a
  build g = g (:) Nil

  splitter :: forall a. List e -> (List e -> a -> a) -> a -> a
  splitter Nil _ n = n
  splitter l c n = l `c` splitter (drop i l) c n