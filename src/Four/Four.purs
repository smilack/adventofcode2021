module AdventOfCode.Twenty21.Four
  ( main
  ) where

import Prelude
import Data.Array (filter, concat, any, null, delete)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.List (List(..), (:), head, tail, fromFoldable, transpose, toUnfoldable, find)
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
    winner = findWinner draws boards
    score = calculateScore winner
  liftEffect do
    log "Draws:"
    logShow draws
    log "Winning board:"
    logShow $ winner.board.rows
    log "Score:"
    logShow $ score

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

type Winner = { board :: Board, draw :: Int }

findWinner :: List Int -> List Board -> Winner
findWinner = go
  where
  go Nil _ = { board: { rows: [], cols: [] }, draw: 0 }
  go (draw : draws) boards =
    let
      boardsWithoutDraw = map (remove draw) boards
      winner = find hasWon boardsWithoutDraw
    in
      case winner of
        Just board -> { board, draw }
        Nothing -> go draws boardsWithoutDraw

  remove draw { rows, cols } =
    { rows: map (delete draw) rows
    , cols: map (delete draw) cols
    }

  hasWon board = any null $ board.rows <> board.cols

calculateScore :: Winner -> Int
calculateScore { board, draw } = uncalledSum * draw
  where
  uncalledSum = sum $ concat board.rows