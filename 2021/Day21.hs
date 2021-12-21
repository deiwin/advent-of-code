module Day21 (main) where

import Data.Either (fromLeft)
import Control.Monad (foldM)
import Control.Applicative (empty, (<|>))
import Control.Arrow (second, (>>>))
import Control.Monad (guard)
import Criterion.Main
  ( bench,
    defaultMain,
    whnf,
  )
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import qualified Data.Char as C
import Data.Function ((&))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Ix
  ( inRange,
    range,
  )
import Data.List
  ( foldl',
    foldl1',
    isPrefixOf,
    iterate,
  )
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
  ( catMaybes,
    fromJust,
    isJust,
  )
import Data.Ord (comparing)
import Data.Sequence
  ( Seq (..),
    (<|),
    (|>),
  )
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import Data.Void (Void)
import Debug.Trace
  ( traceShow,
    traceShowId,
  )
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P
import Text.Printf (printf)

data Player = Player
  { score :: !Int,
    pos :: !Int
  }
  deriving (Eq, Show)

data State = State
  { currentPlayer :: !Player,
    nextPlayer :: !Player,
    turnsTaken :: !Int,
    nextRolls :: [Int]
  }
  deriving (Eq)

instance Show State where
  show state =
    printf
      "State { currentPlayer = %s, nextPlayer = %s, turnsTaken = %d, nextRolls = %d.. }"
      (show (currentPlayer state))
      (show (nextPlayer state))
      (turnsTaken state)
      (head (nextRolls state))

parse :: String -> (Int, Int)
parse input = run $ do
  p1 <- P.string "Player 1 starting position: " *> number <* eol
  p2 <- P.string "Player 2 starting position: " *> number <* eol <* P.eof
  return (p1, p2)
  where
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: _
solve1 = finalScore . runEither playTurn  . initialState
  where
    finalScore :: State -> Int
    finalScore state = score (currentPlayer state) * (3 * turnsTaken state)

initialState :: (Int, Int) -> State
initialState input =
  State
    { currentPlayer = p1,
      nextPlayer = p2,
      turnsTaken = 0,
      nextRolls = cycle [1 .. 100]
    }
  where
    p1 = Player {pos = p1Pos, score = 0}
    p2 = Player {pos = p2Pos, score = 0}
    (p1Pos, p2Pos) = input

runEither :: (a -> Either a a) -> a -> a
runEither f x = fromLeft undefined (foldM (\x _ -> f x) x [0..])

playTurn :: State -> Either State State
playTurn state
  | score newNextPlayer >= 1000 = Left newState
  | otherwise = Right newState
  where
    newState =
      State
        { currentPlayer = nextPlayer state,
          nextPlayer = newNextPlayer,
          turnsTaken = (turnsTaken state) + 1,
          nextRolls = newNextRolls
        }
    newNextPlayer = Player { score = newScore, pos = newPos }
    newScore = score (currentPlayer state) + newPos
    newPos = move (pos (currentPlayer state)) (sum rolls)
    move x d = ((x + d - 1) `mod` 10) + 1
    (rolls, newNextRolls) = L.splitAt 3 (nextRolls state)

main = do
  input <- readFile "inputs/Day21.txt"
  -- exampleInput <- readFile "inputs/Day21_example.txt"
  -- print $ solve1 $ parse input
  print $ solve1 (4, 8)
  runTestTT $
    TestCase $ do
      solve1 (4, 8) @?= 739785
      solve1 (parse input) @?= 512442
