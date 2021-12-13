module AdventOfCode.Twenty21.Eleven.Grid
  ( Grid
  , Adjacent
  , empty
  , alone
  , singleton
  , fromFoldables
  , fromMaybe
  ) where

import Prelude
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))

data Grid a
  = Edge
  | Cell a (Adjacent a)

type Adjacent a =
  { north :: Grid a
  , northEast :: Grid a
  , east :: Grid a
  , southEast :: Grid a
  , south :: Grid a
  , southWest :: Grid a
  , west :: Grid a
  , northWest :: Grid a
  }

empty :: forall a. Grid a
empty = Edge

alone :: forall a. Adjacent a
alone =
  { north: Edge
  , northEast: Edge
  , east: Edge
  , southEast: Edge
  , south: Edge
  , southWest: Edge
  , west: Edge
  , northWest: Edge
  }

singleton :: forall a. a -> Grid a
singleton a = Cell a alone

fromFoldables
  :: forall a f
   . Foldable f
  => f (f a)
  -> Maybe (Grid a)
fromFoldables _ = Nothing

fromMaybe :: forall a. Maybe (Grid a) -> Grid a
fromMaybe = case _ of
  Just grid -> grid
  Nothing -> empty