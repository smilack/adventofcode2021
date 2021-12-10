module AdventOfCode.Twenty21.Nine where

import Prelude
import Data.Array ((!!), catMaybes, take, reverse, sort, groupAll, length, replicate, concat, range)
import Data.Array.NonEmpty (toArray)
import Data.Foldable (minimum, foldr)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Int (fromString)
import Data.List (List(..), (:))
import Data.List (fromFoldable, toUnfoldable) as L
import Data.Map (Map, empty, values, member, insert, union, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Utils (toCharArray, lines)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import PointFree ((<..))

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
adjacentValues = catMaybes <.. (flap <<< getAll)

getAll :: Point -> Array (Array (Array Int) -> Maybe Int)
getAll = map get <<< adjacentPoints

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

lowestNeighbor :: Point -> Array (Array Int) -> Point
lowestNeighbor p heightmap =
  if isLowPoint p then
    p
  else
    let
      points = 
    in

identifyBasins :: Array (Array Int) -> Map Point Point
identifyBasins heightmap = go points empty
  where
  go :: List Point -> Map Point Point -> Map Point Point
  go Nil m = m
  go (p : ps) m = go ps (identify p Nil m)

  identify :: Point -> List Point -> Map Point Point -> Map Point Point
  identify p ps m =
    if p `member` m then
      union m
        $ mkMap (fromMaybe nowhere $ lookup p m) ps
    else if isLowPoint p heightmap then
      union m $ mkMap p (p : ps)
    else
      let
        nextP = lowestNeighbor p heightmap
      in
        identify nextP (p : ps) m

  mkMap :: Point -> List Point -> Map Point Point
  mkMap target ps = fromFoldable $ map (\p -> p /\ target) ps

  nowhere = { x: -1, y: -1 }

  xs = range 0 (length (fromMaybe [] $ heightmap !! 0) - 1)
  ys = range 0 (length heightmap - 1)
  points = L.fromFoldable $ product (\x y -> { x, y }) xs ys

productOfLargestBasins :: Map Point Point -> Int
productOfLargestBasins =
  foldr (*) 1
    <<< take 3
    <<< reverse
    <<< sort
    <<< map length
    <<< map toArray
    <<< groupAll
    <<< L.toUnfoldable
    <<< values

applyAll :: forall a. Array (a -> a) -> a -> a
applyAll = go <<< L.fromFoldable
  where
  go Nil = identity
  go (f : fs) = go fs <<< f

product
  :: forall x y p
   . (x -> y -> p)
  -> Array x
  -> Array y
  -> Array p
product f xs ys = concat $ map (\x -> map (f x) ys) xs