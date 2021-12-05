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

