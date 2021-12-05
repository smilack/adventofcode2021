module Test.AdventOfCode.Twenty21.Five
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty21.Five
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Test.QuickCheck

main :: Effect Unit
main = do
  log "Five"
  log "parseLine"
  quickCheck testParseLine

testParseLine :: Int -> Int -> Int -> Int -> Boolean
testParseLine x1 y1 x2 y2 =
  ({ x1, y1, x2, y2 } == _) $ parseLine $ toString x1 y1 x2 y2
  where
  toString x1 y1 x2 y2 =
    show x1 <> "," <> show y1 <> " -> " <> show x2 <> "," <> show y2