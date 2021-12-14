module Test.AdventOfCode.Twenty21.Eleven.Grid
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty21.Eleven.Grid
import Data.Foldable (foldl, foldr, foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.QuickCheck ((===), Result)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, class Coarbitrary, genericCoarbitrary, arbitraryRecord)
import Test.QuickCheck.Gen (perturbGen)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Prim.RowList (Cons, Nil)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Grid" do
    functorLaws
    functorWithIndexLaws
    foldingAndMapping
    getAndSet
    arrayConstruction

functorLaws :: Spec Unit
functorLaws = do
  describe "Functor laws" do
    it "Identity" $ quickCheck idLaw
    it "Composition" $ quickCheck compLaw

  where
  idLaw :: Grid Int -> Result
  idLaw grid = map identity grid === identity grid

  compLaw
    :: (Number -> String)
    -> (Int -> Number)
    -> Grid Int
    -> Result
  compLaw f g grid =
    map f (map g grid) === map (f <<< g) grid

functorWithIndexLaws :: Spec Unit
functorWithIndexLaws = do
  describe "FunctorWithIndex laws" do
    it "Identity" $ quickCheck idWILaw
    it "Composition" $ quickCheck compWILaw

  where
  idWILaw :: Grid Int -> Result
  idWILaw grid =
    mapWithIndex (const identity) grid === identity grid

  compWILaw
    :: (Point' -> Number -> String)
    -> (Point' -> Int -> Number)
    -> Grid Int
    -> Result
  compWILaw f g grid =
    mapWithIndex (f <<< Point') (mapWithIndex (g <<< Point') grid)
      === mapWithIndex (\i -> f (Point' i) <<< g (Point' i)) grid

newtype Point' = Point' Point

instance Coarbitrary Point' where
  coarbitrary (Point' { x, y }) = perturbGen <<< toNumber $ x * y

foldingAndMapping :: Spec Unit
foldingAndMapping = do
  describe "Folding and Mapping" do
    it "Map add5" do
      map (_ <> Additive 5) grid1 `shouldEqual` grid2
    it "Fold Additive" do
      foldMap identity grid1 `shouldEqual` (Additive 0)
      foldMap identity grid2 `shouldEqual` (Additive 20)
    it "FoldMapWithIndex Point" do
      foldMapWithIndex valueToCoords grid1 `shouldEqual` "(0, 0)(1, 0)(0, 1)(1, 1)"
    it "foldl vs foldr" do
      foldl (\acc val -> acc <> show val) "" gridAsc `shouldEqual` "0123"
      foldr (\val acc -> show val <> acc) "" gridAsc `shouldEqual` "0123"

  where
  grid1 :: Grid (Additive Int)
  grid1 = fromSize { width: 2, height: 2 }

  grid2 :: Grid (Additive Int)
  grid2 = fromSizeWithDefault { width: 2, height: 2 } $ Additive 5

  valueToCoords :: forall a. Point -> a -> String
  valueToCoords { x, y } _ =
    "(" <> show x <> ", " <> show y <> ")"

  gridAsc :: Grid Int
  gridAsc =
    mapWithIndex (\{ x, y } _ -> x + 2 * y)
      $ fromSizeWithDefault { width: 2, height: 2 } 0

getAndSet :: Spec Unit
getAndSet = do
  describe "Get/Set" do
    it "Gets values" do
      get { x: 0, y: 0 } gridAsc1 `shouldEqual` Just 0
      get { x: 1, y: 0 } gridAsc1 `shouldEqual` Just 1
      get { x: 0, y: 1 } gridAsc1 `shouldEqual` Just 2
      get { x: 1, y: 1 } gridAsc1 `shouldEqual` Just 3
      get { x: -1, y: -1 } gridAsc1 `shouldEqual` Nothing
    it "Sets values" do
      let
        gridAsc1' = do
          a <- set { x: 0, y: 0 } 1 gridAsc1
          b <- set { x: 1, y: 0 } 2 a
          c <- set { x: 0, y: 1 } 3 b
          set { x: 1, y: 1 } 4 c
        gridAsc1'' = join $ map (set { x: -1, y: -1 } 5) gridAsc1'
      gridAsc1' `shouldEqual` Just gridAsc2
      gridAsc1'' `shouldEqual` Nothing

gridAsc1 :: Grid Int
gridAsc1 =
  mapWithIndex (\{ x, y } _ -> x + 2 * y)
    $ fromSizeWithDefault { width: 2, height: 2 } 0

gridAsc2 :: Grid Int
gridAsc2 =
  mapWithIndex (\{ x, y } v -> x + 2 * y + v)
    $ fromSizeWithDefault { width: 2, height: 2 } 1

arrayConstruction :: Spec Unit
arrayConstruction = do
  it "Constructs from valid arrays" do
    fromArrays [ [ 0, 1 ], [ 2, 3 ] ] `shouldEqual` Just gridAsc1
    fromArrays [ [ 1, 2 ], [ 3, 4 ] ] `shouldEqual` Just gridAsc2
  it "Rejects non-square matrices" do
    fromArrays [ [ 0 ], [ 1, 2 ] ] `shouldEqual` Nothing
  it "Rejects empty arrays" do
    fromArrays ([ [] ] :: Array (Array Int)) `shouldEqual` Nothing
    fromArrays ([] :: Array (Array Int)) `shouldEqual` Nothing