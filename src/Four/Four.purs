module AdventOfCode.Twenty21.Four
  ( main
  ) where

import Prelude
import Data.Array (filter)
import Data.Int (fromString)
import Data.List (List(..), (:), head, tail, fromFoldable, transpose, toUnfoldable, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

-- Part One: Play bingo! Figure out which board wins first (vert or hori only),
--           add up the uncalled numbers on the winner, and multiply the sum by
--           the number called last.

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Four/input"
  let
    lines = fromFoldable $ split (Pattern "\n") input
    draws = getDraws $ head $ lines
    boards = getBoards $ tail $ lines
  liftEffect do
    log "Draws:"
    logShow draws
    log "Board 1:"
    logShow $ head boards
    log "Num boards:"
    logShow $ length boards

getDraws :: Maybe String -> List Int
getDraws = case _ of
  Nothing -> Nil
  Just csv ->
    let
      mDraws = map fromString $ fromFoldable $ split (Pattern ",") csv
    in
      fromMaybe Nil $ sequence mDraws

type Board = { rows :: Array (Array Int), cols :: Array (Array Int) }

getBoards :: Maybe (List String) -> List Board
getBoards = case _ of
  Nothing -> Nil
  Just lines -> go lines
  where
  go (_ : a : b : c : d : e : rest) = mkBoard a b c d e : go rest
  go _ = Nil

  mkBoard a b c d e =
    let
      rows = map parseRow [ a, b, c, d, e ]
    in
      { rows, cols: transposeA rows }

  parseRow =
    fromMaybe []
      <<< sequence
      <<< filter (_ /= Nothing)
      <<< map fromString
      <<< split (Pattern " ")

transposeA :: forall a. Array (Array a) -> Array (Array a)
transposeA =
  map toUnfoldable
    <<< toUnfoldable
    <<< transpose
    <<< map fromFoldable
    <<< fromFoldable

-- plan: As each number is drawn, filter it from all boards (all rows and cols
--  of each board). If any row or col is empty, that board wins. Pick the rows
--  or cols of that board and sum them up