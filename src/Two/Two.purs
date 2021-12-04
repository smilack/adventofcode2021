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

-- Part Two: Same as above except commands have non-intuitive meaning:
--             * "aim" starts at 0
--             * up and down affect aim
--             * forward increases forward position by N and depth by N*aim

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./src/Two/input"
  let
    tokens = readCommandArray input
    commands = parseCommands tokens
    position = unwrap $ foldl (<>) mempty commands
    position2 = runCommands2 tokens
  liftEffect do
    log "Part 1:"
    log "Position is:"
    logShow position
    log "Depth × Forward is:"
    logShow (position.depth * position.forward)
    log "Part 2:"
    log "Position is:"
    logShow position2
    log "Depth × Forward is:"
    logShow (position2.depth * position2.forward)

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

type AimCommand = { depth :: Int, forward :: Int, aim :: Int }

runCommands2 :: Array CommandToken -> AimCommand
runCommands2 =
  foldl f { depth: 0, forward: 0, aim: 0 }
  where
  f position@{ depth, forward, aim } (CommandToken s n)
    | s == "up" = position { aim = aim - n }
    | s == "down" = position { aim = aim + n }
    | s == "forward" =
        position { depth = depth + aim * n, forward = forward + n }
    | otherwise = position