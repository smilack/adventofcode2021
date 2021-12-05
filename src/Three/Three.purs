module AdventOfCode.Twenty21.Three
  ( main
  ) where

import Prelude
import Data.Int (binary, fromStringAs)
import Data.List (List(..), (:), fromFoldable, transpose, length, toUnfoldable, dropEnd, find)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (split)
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

-- Part One: Given a list of binary numbers of equal length, find the most and
--           least common digit in each column. Construct new binary numbers,
--           one from the most common digits and one from the least common, and
--           return their product.

-- Part Two: Find the numbers as above except:
--             * For the most common, use 1 if there are equal 0s and 1s
--             * For the least common, use 0 if there are equal 0s and 1s
--           Then, find the two entries that most closely match these numbers
--           (starting from the left). Return the product of those two entries.

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
    mcds = map (mostCommonWithPreference '1') frequencies
    mci = binaryListToInt $ findClosest mcds lists
    lcds = invertBinary mcds
    lci = binaryListToInt $ findClosest lcds lists
  liftEffect do
    log "Part 1:"
    log "Most common digits:"
    logShow mostCommonInt
    log "Least common digits:"
    logShow leastCommonInt
    log "Product:"
    logShow $ mostCommonInt * leastCommonInt
    log "Part 2:"
    log "Closest to most common digits:"
    logShow mci
    log "Closest to least common digits:"
    logShow lci
    log "Product:"
    logShow $ mci * lci
    log "Debug:"
    log "Most:"
    logShow mostCommonDigs
    logShow mcds
    log "Least:"
    logShow leastCommonDigs
    logShow lcds
    log "Any evens?"
    logShow $ map isEven frequencies

isEven :: List Char -> Char
isEven xs = if count '0' xs == count '1' xs then 'y' else 'n'

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

mostCommonWithPreference :: Char -> List Char -> Char
mostCommonWithPreference preferred xs =
  if zeroes > half then
    '0'
  else if zeroes == half then
    preferred
  else
    '1'
  where
  zeroes = count '0' xs
  half = length xs / 2

findClosest :: forall a. Eq a => List a -> List (List a) -> List a
findClosest needle haystack =
  case find (match needle) haystack of
    Just closest -> closest
    Nothing -> findClosest (dropEnd 1 needle) haystack

  where
  match :: List a -> List a -> Boolean
  match (a : as) (b : bs) =
    if a == b then
      match as bs
    else
      false
  match _ _ = true