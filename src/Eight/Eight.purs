module AdventOfCode.Twenty21.Eight where

import Prelude
import AdventOfCode.Twenty21.Eight.Segment (Signal, fromString, empty)
import Data.Array (elem, length, filter, concat, find, catMaybes, reverse)
import Data.Foldable (sum)
import Data.FoldableWithIndex (foldlWithIndexDefault)
import Data.Int (pow)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (fromMaybe)
import Data.Set (size, properSubset, intersection, difference, union)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines, words)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

-- Part one: Given signal patterns for a number of mixed-up seven-segment displays,
--           count the number of 1s, 4s, 7s, and 8s, in the output

-- Part two: Identify each digit of the mixed up displays to decode the output, then
--           return the sum of all output values

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Eight/input"
  liftEffect do
    log "Part 1:"
    log "Number of 1s, 4s, 7s, 8s:"
    logShow $ countMatchingOutputDigits is1478 $ parseInput input
    log "Part 2:"
    log "Sum of output:"
    logShow $ collectData $ parseInput input

type Reading = { digits :: Array Signal, output :: Array Signal }

parseInput :: String -> Array Reading
parseInput = map parseReading <<< map (split (Pattern " | ")) <<< lines
  where
  parseReading [ d, o ] = { digits: parseSignals d, output: parseSignals o }
  parseReading _ = { digits: [], output: [] }

  parseSignals = map fromString <<< words

countMatchingOutputDigits :: (Signal -> Boolean) -> Array Reading -> Int
countMatchingOutputDigits f = length <<< filter f <<< concat <<< map (_.output)

is1478 :: Signal -> Boolean
is1478 = (_ `elem` [ 2, 4, 3, 7 ]) <<< size

type Key = Map Signal Int

solve :: Reading -> Key
solve { digits } =
  fromFoldable
    [ zero /\ 0
    , one /\ 1
    , two /\ 2
    , three /\ 3
    , four /\ 4
    , five /\ 5
    , six /\ 6
    , seven /\ 7
    , eight /\ 8
    , nine /\ 9
    ]
  where
  one = select ((_ == 2) <<< size)
  four = select ((_ == 4) <<< size)
  seven = select ((_ == 3) <<< size)
  eight = select ((_ == 7) <<< size)
  nine = select (\s -> size s == 6 && four `properSubset` s)
  three = select (\s -> size s == 5 && one `properSubset` s)
  five = select (\s -> size s == 5 && (four `difference` one) `properSubset` s)
  six = five `union` (eight `difference` nine)
  zero = eight `difference` ((four `intersection` three) `difference` one)
  two = select (\s -> size s == 5 && s /= three && s /= five)

  select f = fromMaybe empty $ find f digits

-- # of segments per digit:
-- digit    0 1 2 3 4 5 6 7 8 9
-- segments 6 2 5 5 4 5 6 3 7 6

-- digit id rules:
-- 1 has 2 segments
-- 4 has 4 segments
-- 7 has 3 segments
-- 8 has 7 segments
-- 9 has 6 segments and is a superset of 4
-- 3 has 5 segments and is a superset of 1
-- 5 has 5 segments and is a superset of (4 difference 1)
-- 6 is 5 union (8 difference 9)
-- 0 is 8 difference ((4 intersection 3) difference 1)
-- 2 has 5 segments and is not 3 or 5

decodeOutput :: Reading -> Int
decodeOutput reading = digitsToNum $ catMaybes $ map (\k -> lookup k key) reading.output
  where
  key = solve reading

collectData :: Array Reading -> Int
collectData = sum <<< map decodeOutput

digitsToNum :: Array Int -> Int
digitsToNum = foldlWithIndexDefault addDigit 0 <<< reverse
  where
  addDigit place total digit = total + (digit * 10 `pow` place)