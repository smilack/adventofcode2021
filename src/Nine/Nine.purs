module AdventOfCode.Twenty21.Nine where

import Prelude
import Data.Array ((!!), catMaybes)
import Data.Foldable (minimum)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Int (fromString)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Utils (toCharArray, lines)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Nine/input"
  liftEffect do
    log "Part 1:"
    log "Risk level:"
    logShow $ assessRisk $ parseInput input

parseInput :: String -> Array (Array Int)
parseInput = map (catMaybes <<< map fromString <<< toCharArray) <<< lines

type Point = { x :: Int, y :: Int }

get :: Point -> Array (Array Int) -> Maybe Int
get { x, y } = join <<< map (_ !! x) <<< (_ !! y)

adjacentValues :: Point -> Array (Array Int) -> Array Int
adjacentValues p = catMaybes <<< flap getAll
  where
  getAll :: Array (Array (Array Int) -> Maybe Int)
  getAll = map get $ adjacentPoints p

adjacentPoints :: Point -> Array Point
adjacentPoints { x, y } =
  [ { x: x + 1, y }
  , { x: x - 1, y }
  , { x, y: y + 1 }
  , { x, y: y - 1 }
  ]

isLowPoint :: Point -> Array (Array Int) -> Boolean
isLowPoint p a = case get p a of
  Nothing -> false
  Just v -> v < fromMaybe 9 (minimum $ adjacentValues p a)

assessRisk :: Array (Array Int) -> Int
assessRisk heightmap = foldrWithIndex searchRow 0 heightmap
  where
  searchRow :: Int -> Array Int -> Int -> Int
  searchRow y row total = foldrWithIndex (checkCell y) total row

  checkCell :: Int -> Int -> Int -> Int -> Int
  checkCell y x v = if isLowPoint { x, y } heightmap then (_ + v + 1) else identity

type Basin = { nadir :: Point, cells :: List Point }

findBasins :: Array (Array Int) -> Array Basin
findBasins = go []
  where
  go :: Array (Array (Maybe Point)) -> Array Basin -> Array Basin
  go points basins =

  addPoint :: Point -> Basin -> Basin
  addPoint p b = b { cells = p : b.cells }

  findNadir :: Point ->