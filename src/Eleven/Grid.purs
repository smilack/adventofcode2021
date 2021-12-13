module AdventOfCode.Twenty21.Eleven.Grid
  ( Grid
  -- , Adjacent
  -- , empty
  -- , alone
  -- , singleton
  -- , fromFoldables
  -- , fromMaybe
  ) where

import Prelude
import Data.Array.NonEmpty (NonEmptyArray, (!!), replicate, range, updateAt)
import Data.Foldable (class Foldable, foldl, foldr, foldMap, foldrDefault, foldlDefault)
import Data.Maybe (Maybe(..))

type Point = { x :: Int, y :: Int }

type Size = { width :: Int, height :: Int }

data Grid a = Grid Size (NonEmptyArray (NonEmptyArray a))

fromSizeWithDefault :: forall a. Size -> a -> Grid a
fromSizeWithDefault size@{ width, height } a = Grid size rows
  where
  row :: NonEmptyArray a
  row = replicate width a

  rows :: NonEmptyArray (NonEmptyArray a)
  rows = replicate height row

fromSize :: forall m. Monoid m => Size -> Grid m
fromSize size = fromSizeWithDefault size mempty

get :: forall a. Point -> Grid a -> Maybe a
get { x, y } (Grid _ rows) = join $ map (_ !! x) $ (rows !! y)

set :: forall a. Point -> a -> Grid a -> Maybe (Grid a)
set { x, y } a (Grid size rows) = grid'
  where
  -- grid' = map (\rs' -> Grid size rows') rows'

  -- rows' :: Maybe (Maybe (NonEmptyArray (NonEmptyArray a)))
  -- rows' = map (\r' -> updateAt y r' rows) row'

  -- row' :: Maybe (NonEmptyArray a)
  -- row' = join $ map (updateAt x a) $ (rows !! y)

  grid' :: Maybe (Grid a)
  grid' = do
    row <- rows !! y
    row' <- updateAt x a row
    rows' <- updateAt y row' rows
    pure $ Grid size rows'

-- instance Foldable Grid where
--   foldMap f g = 
--   foldl = foldlDefault
--   foldr = foldrDefault

-- instance Functor Grid where
--   map f g =

----------------- old stuff -----------------

-- data Grid a
--   = Edge
--   | Cell a (Adjacent a)

-- type Adjacent a =
--   { north :: Grid a
--   , northEast :: Grid a
--   , east :: Grid a
--   , southEast :: Grid a
--   , south :: Grid a
--   , southWest :: Grid a
--   , west :: Grid a
--   , northWest :: Grid a
--   }

-- empty :: forall a. Grid a
-- empty = Edge

-- alone :: forall a. Adjacent a
-- alone =
--   { north: Edge
--   , northEast: Edge
--   , east: Edge
--   , southEast: Edge
--   , south: Edge
--   , southWest: Edge
--   , west: Edge
--   , northWest: Edge
--   }

-- singleton :: forall a. a -> Grid a
-- singleton a = Cell a alone

-- fromFoldables
--   :: forall a f
--    . Foldable f
--   => f (f a)
--   -> Maybe (Grid a)
-- fromFoldables _ = Nothing

-- fromMaybe :: forall a. Maybe (Grid a) -> Grid a
-- fromMaybe = case _ of
--   Just grid -> grid
--   Nothing -> empty