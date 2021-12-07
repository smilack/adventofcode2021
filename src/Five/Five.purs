module AdventOfCode.Twenty21.Five where

import Prelude
import Data.Array (concat) as A
import Data.Int (fromString)
import Data.List (List(..), (:), filter, fromFoldable, catMaybes, range, concat, length)
import Data.Map (Map, empty, insertWith, values)
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
    axialLines = catMaybes $ map toAxial lines
    allPoints = concat $ map enumerate axialLines
    spots = tallySpots allPoints
    nHotspots = countHotspots spots

  liftEffect do
    log "Part One:"
    log "Number of spots:"
    logShow nHotspots

parseLines :: String -> List Line
parseLines = fromFoldable <<< map parseLine <<< split (Pattern "\n")

parseLine :: String -> Line
parseLine =
  mkLine
    <<< sequence
    <<< map fromString
    <<< A.concat
    <<< map (split (Pattern ","))
    <<< split (Pattern " -> ")

type Line = { x1 :: Int, y1 :: Int, x2 :: Int, y2 :: Int }

data AxialLine
  = Hori { x1 :: Int, x2 :: Int, y :: Int }
  | Verti { x :: Int, y1 :: Int, y2 :: Int }

type Point = { x :: Int, y :: Int }

mkLine :: Maybe (Array Int) -> Line
mkLine (Just [ x1, y1, x2, y2 ]) = { x1, y1, x2, y2 }
mkLine _ = { x1: 0, y1: 0, x2: 0, y2: 0 }

toAxial :: Line -> Maybe AxialLine
toAxial { x1, y1, x2, y2 } =
  if x1 == x2 then
    Just $ Verti { x: x1, y1, y2 }
  else if y1 == y2 then
    Just $ Hori { x1, x2, y: y1 }
  else
    Nothing

enumerate :: AxialLine -> List Point
enumerate = case _ of
  Hori { x1, x2, y } -> map (\x -> { x, y }) $ range x1 x2
  Verti { x, y1, y2 } -> map (\y -> { x, y }) $ range y1 y2

tallySpots :: List Point -> Map Point Int
tallySpots = go empty
  where
  go m Nil = m
  go m (p : ps) = go (insertWith (+) p 1 m) ps

countHotspots :: Map Point Int -> Int
countHotspots =
  length
    <<< filter (_ > 1)
    <<< values