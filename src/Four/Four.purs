module AdventOfCode.Twenty21.Four
  ( main
  ) where

import Prelude
import Data.Array (concat, any, delete)
import Data.Array (null, filter) as A
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.List (List(..), (:), head, tail, fromFoldable, transpose, toUnfoldable, find)
import Data.List (null, filter) as L
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

-- Part Two: Same except find the LAST winning board

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Four/input"
  let
    lines = fromFoldable $ split (Pattern "\n") input
    draws = getDraws $ head $ lines
    boards = getBoards $ tail $ lines
    winner = findWinner draws boards
    score = calculateScore winner
    lastWinner = findLastWinner draws boards
  liftEffect do
    log "Part 1:"
    log "Draws:"
    logShow draws
    log "Winning board:"
    logShow $ winner.board.rows
    log "Score:"
    logShow $ score
    log "Part 2:"
    log "Last winning board:"
    logShow $ lastWinner.board.rows
    log "Score:"
    logShow $ calculateScore lastWinner

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
      <<< A.filter (_ /= Nothing)
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
  go Nil _ = winnerError
  go (draw : draws) boards =
    let
      boardsWithoutDraw = map (remove draw) boards
      winner = find hasWon boardsWithoutDraw
    in
      case winner of
        Just board -> { board, draw }
        Nothing -> go draws boardsWithoutDraw

findLastWinner :: List Int -> List Board -> Winner
findLastWinner = go
  where
  go Nil _ = winnerError
  go (draw : draws) boards =
    let
      boardsWithoutDraw = map (remove draw) boards
      winner = find hasWon boardsWithoutDraw
      nonWinners = L.filter (not <<< hasWon) boardsWithoutDraw
    in
      if L.null nonWinners then
        case winner of
          Just board -> { board, draw }
          Nothing -> winnerError
      else
        go draws nonWinners

winnerError :: Winner
winnerError = { board: { rows: [], cols: [] }, draw: 0 }

remove :: Int -> Board -> Board
remove draw { rows, cols } =
  { rows: map (delete draw) rows
  , cols: map (delete draw) cols
  }

hasWon :: Board -> Boolean
hasWon board = any A.null $ board.rows <> board.cols

calculateScore :: Winner -> Int
calculateScore { board, draw } = uncalledSum * draw
  where
  uncalledSum = sum $ concat board.rows