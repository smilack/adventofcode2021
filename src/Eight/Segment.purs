module AdventOfCode.Twenty21.Eight.Segment
  ( Segment(..)
  , Signal
  , toString
  , fromString
  , signal
  ) where

import Prelude

import Data.Array (catMaybes)
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, foldr)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Ord.Generic (genericCompare)
import Data.Set (Set, fromFoldable)
import Data.Show.Generic (genericShow)
import Data.String (toLower)
import Data.String.Utils (toCharArray)

type Signal = Set Segment

signal :: forall f. Foldable f => f Segment -> Signal
signal = fromFoldable

toString :: Signal -> String
toString = foldr ((<>) <<< toLower <<< show) ""

fromString :: String -> Signal
fromString = fromFoldable <<< catMaybes <<< map readSegment <<< toCharArray

readSegment :: String -> Maybe Segment
readSegment = case _ of
  "a" -> Just A
  "b" -> Just B
  "c" -> Just C
  "d" -> Just D
  "e" -> Just E
  "f" -> Just F
  "g" -> Just G
  _ -> Nothing

data Segment = A | B | C | D | E | F | G

derive instance Generic Segment _

instance Eq Segment where
  eq = genericEq

instance Ord Segment where
  compare = genericCompare

instance Show Segment where
  show = genericShow

