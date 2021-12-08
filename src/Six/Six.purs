module AdventOfCode.Twenty21.Six where

import Prelude
import Control.Parallel (parSequence)
import Data.Array (modifyAt)
import Data.BigInt (BigInt, fromInt)
import Data.Foldable (sum)
import Data.Function (applyN)
import Data.Int (fromString)
import Data.List (List(..), (:), fromFoldable, catMaybes, length, take, drop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
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
  let
    nFish = sum nFishChunks
    track = fillTrack ages
    track' = ageBy 256 track
    nFish' = countFish track'
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
    logShow track'

parseAges :: String -> List Int
parseAges =
  catMaybes
    <<< map fromString
    <<< fromFoldable
    <<< split (Pattern ",")

asyncSimulate :: Int -> List Int -> Aff Int
asyncSimulate = (pure <<< length) <.. simulate

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

newtype FishTrack = FishTrack (Array BigInt)

derive instance Newtype FishTrack _

derive instance Eq FishTrack

instance Show FishTrack where
  show (FishTrack t) = "FishTrack " <> show t

emptyTrack ∷ FishTrack
emptyTrack = FishTrack (map fromInt [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ])

addFish :: Int -> FishTrack -> FishTrack
addFish f t@(FishTrack track)
  | f >= 0 && f <= 8 = case modifyAt f (_ + (fromInt 1)) track of
      Just track' -> FishTrack track'
      Nothing -> t
  | otherwise = t

fillTrack :: List Int -> FishTrack
fillTrack = go
  where
  go Nil = emptyTrack
  go (x : xs) = addFish x $ go xs

age :: FishTrack -> FishTrack
age (FishTrack [ d0, d1, d2, d3, d4, d5, d6, d7, d8 ]) =
  FishTrack [ d1, d2, d3, d4, d5, d6, d7 + d0, d8, d0 ]
age err = err

countFish :: FishTrack -> BigInt
countFish (FishTrack a) = sum a

ageBy :: Int -> FishTrack -> FishTrack
ageBy = applyN age