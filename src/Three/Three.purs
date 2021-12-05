module AdventOfCode.Twenty21.Three
  ( main
  ) where

import Prelude
import Data.Int (binary, fromStringAs)
import Data.List (List(..), (:), fromFoldable, transpose, length, toUnfoldable, filter, index)
import Data.Maybe (fromMaybe)
import Data.String (split)
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import PointFree ((<..))

-- Part One: Given a list of binary numbers of equal length, find the most and
--           least common digit in each column. Construct new binary numbers,
--           one from the most common digits and one from the least common, and
--           return their product.

-- Part Two: Find the numbers as above except:
--             * For the most common, use 1 if there are equal 0s and 1s
--             * For the least common, use 0 if there are equal 0s and 1s
--             * Calculate each bit after eliminating the numbers that don't
--               match the previous bit
--           Return the product of the two remaining numbers.

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Three/input"
  let
    arrays = getBinaryArrays input
    lists = nestedArraysToLists arrays
    frequencies = transpose lists
    mostCommonDigs = map getMostCommonDigit frequencies
    mostCommonInt = binaryListToInt mostCommonDigs
    leastCommonDigs = invertBinary mostCommonDigs
    leastCommonInt = binaryListToInt leastCommonDigs
    -- part two
    most = binaryListToInt $ findMatch getMostCommonDigit lists
    least = binaryListToInt $ findMatch getLeastCommonDigit lists

  liftEffect do
    log "Part 1:"
    log "Most common digits:"
    logShow mostCommonInt
    log "Least common digits:"
    logShow leastCommonInt
    log "Product:"
    logShow $ mostCommonInt * leastCommonInt
    log "Part 2:"
    log "Most:"
    logShow most
    log "Least:"
    logShow least
    log "Product:"
    logShow $ most * least

getBinaryArrays :: String -> Array (Array Char)
getBinaryArrays = map toCharArray <<< split (Pattern "\n")

nestedArraysToLists :: forall a. Array (Array a) -> List (List a)
nestedArraysToLists = map fromFoldable <<< fromFoldable

count :: forall a. Eq a => a -> List a -> Int
count = go 0
  where
  go n _ Nil = n
  go n a (x : xs) = if a == x then go (n + 1) a xs else go n a xs

getMostCommonDigit :: List Char -> Char
getMostCommonDigit xs = if count '0' xs > half then '0' else '1'
  where
  half = length xs / 2

getLeastCommonDigit :: List Char -> Char
getLeastCommonDigit xs = if count '1' xs < count '0' xs then '1' else '0'

nth :: Int -> List Char -> Char
nth = fromMaybe '_' <.. flip index

invertBinary :: List Char -> List Char
invertBinary = map f
  where
  f '0' = '1'
  f '1' = '0'
  f _ = '0'

binaryListToInt :: List Char -> Int
binaryListToInt =
  fromMaybe 0
    <<< fromStringAs binary
    <<< fromCharArray
    <<< toUnfoldable

findMatch :: (List Char -> Char) -> List (List Char) -> List Char
findMatch getDigit = go 0
  where
  go _ (x : Nil) = x
  go _ Nil = '0' : Nil
  go n haystack =
    let
      target = getDigit $ map (nth n) haystack
    in
      go (n + 1) $ filter ((_ == target) <<< nth n) haystack