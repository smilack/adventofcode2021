module AdventOfCode.Twenty21.Eight.Segment
  ( Segment(..)
  , Signal
  , toString
  , signal
  ) where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, foldr)
import Data.Generic.Rep (class Generic)
import Data.Ord.Generic (genericCompare)
import Data.Set (Set, fromFoldable)
import Data.Show.Generic (genericShow)
import Data.String (toLower)

type Signal = Set Segment

signal :: forall f. Foldable f => f Segment -> Signal
signal = fromFoldable

toString :: Signal -> String
toString = foldr ((<>) <<< toLower <<< show) ""

data Segment = A | B | C | D | E | F | G

derive instance Generic Segment _

instance Eq Segment where
  eq = genericEq

instance Ord Segment where
  compare = genericCompare

instance Show Segment where
  show = genericShow

