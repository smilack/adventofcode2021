module AdventOfCode.Twenty21.Nine where

import Prelude
import Data.Array ((!!), catMaybes, length)
import Data.Array.NonEmpty (cons')
import Data.Foldable (foldr)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Semigroup.Foldable (minimum)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
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
    log "Number of cells:"
    logShow $ foldr (+) 0 $ map length $ parseInput input
    log "Risk level:"
    --1486 too high
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
  Just v -> v == minimum (v `cons'` adjacentValues p a)

assessRisk :: Array (Array Int) -> Int
assessRisk heightmap = foldrWithIndex searchRow 0 heightmap
  where
  searchRow :: Int -> Array Int -> Int -> Int
  searchRow y row total = foldrWithIndex (checkCell y) total row

  checkCell :: Int -> Int -> Int -> Int -> Int
  checkCell y x v = if isLowPoint { x, y } heightmap then (_ + (v + 1)) else identity