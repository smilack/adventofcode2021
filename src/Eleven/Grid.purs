module AdventOfCode.Twenty21.Eleven.Grid
  ( Grid
  , Point
  , fromSize
  , fromSizeWithDefault
  , fromArrays
  , get
  , set
  , adjacent4
  , adjacent8
  ) where

import Prelude
import Data.Array (nub, head, length, fromFoldable, concat, filter, range)
import Data.Array.NonEmpty (NonEmptyArray, (!!), replicate, updateAt, fromArray)
import Data.Foldable (class Foldable, foldMap, foldrDefault, foldlDefault, fold)
import Data.FoldableWithIndex (class FoldableWithIndex, foldrWithIndexDefault, foldlWithIndexDefault)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Control.Alternative (guard)
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
      show (fromFoldable $ map fromFoldable rows)

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

fromArrays :: forall a. Array (Array a) -> Maybe (Grid a)
fromArrays arrs = do
  guard $ length lens == 1
  size <- msize
  nearrs <- fromArray =<< traverse fromArray arrs
  pure $ Grid size nearrs

  where
  lens = nub $ map length arrs

  msize = do
    let height = length arrs
    width <- head lens
    pure { width, height }

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

adjacent4 :: forall a. Point -> Grid a -> Array Point
adjacent4 { x, y } (Grid size _) =
  filter (valid size) allPoints
  where
  allPoints =
    [ { x: inc x, y }
    , { x: dec x, y }
    , { x, y: inc y }
    , { x, y: dec y }
    ]

adjacent8 :: forall a. Point -> Grid a -> Array Point
adjacent8 p (Grid size _) =
  filter (\p' -> p' /= p && valid size p') allPoints
  where
  allPoints =
    product
      (range (dec p.x) (inc p.x))
      (range (dec p.y) (inc p.y))

  product xs ys =
    concat $ map (\x -> map (\y -> { x, y }) ys) xs

valid :: Size -> Point -> Boolean
valid { width, height } { x, y } =
  x >= 0 && x < width && y >= 0 && y < height

dec :: Int -> Int
dec = (_ - 1)

inc :: Int -> Int
inc = (_ + 1)