module AdventOfCode.Twenty21.Eleven.Grid
  ( Grid
  , Point
  , fromSize
  , fromSizeWithDefault
  , get
  , set
  ) where

import Prelude
import Data.Array (fromFoldable) as Array
import Data.Array.NonEmpty (NonEmptyArray, (!!), replicate, range, updateAt)
import Data.Foldable (class Foldable, foldMap, foldrDefault, foldlDefault, fold)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldrWithIndexDefault, foldlWithIndexDefault)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe(..))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (chooseInt)

-----------
-- Types --
-----------

type Point = { x :: Int, y :: Int }

type Size = { width :: Int, height :: Int }

data Grid a = Grid Size (NonEmptyArray (NonEmptyArray a))

------------------
-- Type Classes --
------------------

instance Eq a => Eq (Grid a) where
  eq (Grid s1 r1) (Grid s2 r2) = s1 == s2 && r1 == r2

instance Show a => Show (Grid a) where
  show (Grid { width, height } rows) =
    "Grid (" <> show width <> " x " <> show height <> ")" <>
      show (Array.fromFoldable $ map Array.fromFoldable rows)

instance Arbitrary a => Arbitrary (Grid a) where
  arbitrary = do
    width <- chooseInt 1 100
    height <- chooseInt 1 100
    value <- arbitrary
    pure $ fromSizeWithDefault { width, height } value

instance Functor Grid where
  map f (Grid size rows) = Grid size (map (map f) rows)

instance FunctorWithIndex Point Grid where
  mapWithIndex :: forall a b. (Point -> a -> b) -> Grid a -> Grid b
  mapWithIndex f (Grid size rows) = Grid size rows'
    where
    rows' = mapWithIndex mapInner rows
    mapInner y row = mapWithIndex (\x a -> f { x, y } a) row

instance Foldable Grid where
  foldMap f (Grid _ rows) = fold (map (foldMap f) rows)
  foldl f b g = foldlDefault f b g
  foldr f b g = foldrDefault f b g

instance FoldableWithIndex Point Grid where
  foldMapWithIndex f grid = foldMap identity $ mapWithIndex f grid
  foldlWithIndex f b g = foldlWithIndexDefault f b g
  foldrWithIndex f b g = foldrWithIndexDefault f b g

------------------
-- Constructors --
------------------

fromSizeWithDefault :: forall a. Size -> a -> Grid a
fromSizeWithDefault size@{ width, height } a = Grid size rows
  where
  row :: NonEmptyArray a
  row = replicate width a

  rows :: NonEmptyArray (NonEmptyArray a)
  rows = replicate height row

fromSize :: forall m. Monoid m => Size -> Grid m
fromSize size = fromSizeWithDefault size mempty

----------------
-- Operations --
----------------

get :: forall a. Point -> Grid a -> Maybe a
get { x, y } (Grid _ rows) = join $ map (_ !! x) $ (rows !! y)

set :: forall a. Point -> a -> Grid a -> Maybe (Grid a)
set { x, y } a (Grid size rows) = grid'
  where
  grid' :: Maybe (Grid a)
  grid' = do
    row <- rows !! y
    row' <- updateAt x a row
    rows' <- updateAt y row' rows
    pure $ Grid size rows'