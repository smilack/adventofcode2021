module AdventOfCode.Twenty21.Eleven where

import Prelude
import AdventOfCode.Twenty21.Eleven.Grid (Grid, fromArrays)
import Data.Eq.Generic (genericEq)
import Data.Int (fromString)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.String.Utils (lines, toCharArray)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Eleven/input"
  liftEffect $ log input

data Octopus
  = Unflashed Int
  | Flashed

instance Show Octopus where
  show (Unflashed i) = show i
  show Flashed = "*"

parseInput :: String -> Maybe (Grid Octopus)
parseInput =
  fromArrays
    <<< map (map mkOcto)
    <<< map toCharArray
    <<< lines
  where
  mkOcto :: String -> Octopus
  mkOcto = Unflashed <<< fromMaybe 0 <<< fromString
