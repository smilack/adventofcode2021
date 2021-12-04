module AdventOfCode.Twenty21.Two
  ( main
  ) where

import Prelude
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

-- Part One: Given a list of commands (up N, down N, forward N), calculate the
--           depth and forward position after following all commands. Return the
--           product of the depth and position.

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Two/input"
  let
    tokens = readCommandArray input
    commands = parseCommands tokens
    position = unwrap $ foldl (<>) mempty commands
  liftEffect do
    log "Position is:"
    logShow position
    log "Depth × Forward is:"
    logShow (position.depth * position.forward)

readCommandArray :: String -> Array CommandToken
readCommandArray =
  map toToken
    <<< map (split (Pattern " "))
    <<< split (Pattern "\n")
  where
  toToken [ s, n ] = CommandToken s (fromMaybe 0 $ fromString n)
  toToken _ = CommandToken "" 0

parseCommands :: Array CommandToken -> Array Command
parseCommands = map go
  where
  go (CommandToken "up" n) = Command { depth: -n, forward: 0 }
  go (CommandToken "down" n) = Command { depth: n, forward: 0 }
  go (CommandToken "forward" n) = Command { depth: 0, forward: n }
  go _ = mempty

data CommandToken = CommandToken String Int

newtype Command = Command { depth :: Int, forward :: Int }

derive instance Newtype Command _

instance Semigroup Command where
  append (Command a) (Command b) =
    Command
      { depth: a.depth + b.depth
      , forward: a.forward + b.forward
      }

instance Monoid Command where
  mempty = Command { depth: 0, forward: 0 }