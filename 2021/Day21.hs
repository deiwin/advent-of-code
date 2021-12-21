{-# LANGUAGE FlexibleContexts #-}

module Day21 (main) where

import Control.Applicative (empty, (<|>))
import Control.Arrow (second, (>>>))
import Control.Monad (foldM, guard)
import Control.Monad.Memo (MonadMemo, for2, memo, runMemo, startEvalMemo)
import Criterion.Main
  ( bench,
    defaultMain,
    whnf,
  )
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import qualified Data.Char as C
import Data.Either (fromLeft)
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
  { pNumber :: !Int,
    score :: !Int,
    pos :: !Int
  }
  deriving (Eq, Ord, Show)

data State = State
  { currentPlayer :: !Player,
    nextPlayer :: !Player
  }
  deriving (Eq, Ord, Show)

data DetState = DetState
  { state :: State,
    turnsTaken :: !Int,
    nextRolls :: [Int]
  }

instance Show DetState where
  show x =
    printf
      "DetState { state = %s, turnsTaken = %d, nextRolls = %d.. }"
      (show (state x))
      (turnsTaken x)
      (head (nextRolls x))

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

solve1 :: (Int, Int) -> Int
solve1 = finalScore . runEither (endCondition playTurnDet) . makeState
  where
    endCondition f s
      | score (nextPlayer (state newS)) >= 1000 = Left newS
      | otherwise = Right newS
      where
        newS = f s
    finalScore detState = score (currentPlayer (state detState)) * (3 * turnsTaken detState)
    makeState input =
      DetState
        { state = initialState input,
          turnsTaken = 0,
          nextRolls = cycle [1 .. 100]
        }

initialState :: (Int, Int) -> State
initialState input = State {currentPlayer = p1, nextPlayer = p2}
  where
    p1 = Player {pNumber = 1, pos = p1Pos, score = 0}
    p2 = Player {pNumber = 2, pos = p2Pos, score = 0}
    (p1Pos, p2Pos) = input

solve2 :: (Int, Int) -> _
solve2 input = uncurry max $ countDiracWins $ initialState input

countDiracWins :: State -> (Int, Int)
countDiracWins = startEvalMemo . go
  where
    go :: MonadMemo State (Int, Int) m => State -> m (Int, Int)
    go s
      | score (nextPlayer s) >= 21 =
        if pNumber (nextPlayer s) == 1
          then return (1, 0)
          else return (0, 1)
      | otherwise = tupleSum <$> mapM (\rs -> memo go (playTurn rs s)) rolls
    rolls =  [[x, y, z] | x <- [1 .. 3], y <- [1 .. 3], z <- [1 .. 3]]
    tupleSum :: [(Int, Int)] -> (Int, Int)
    tupleSum = foldl' tupleAdd (0, 0)
    tupleAdd (a, b) (c, d) = (a + c, b + d)

runEither :: (a -> Either a a) -> a -> a
runEither f x = fromLeft undefined (foldM (\x _ -> f x) x [0 ..])

playTurnDet :: DetState -> DetState
playTurnDet detState =
  DetState
    { state = playTurn rolls (state detState),
      nextRolls = newNextRolls,
      turnsTaken = turnsTaken detState + 1
    }
  where
    (rolls, newNextRolls) = L.splitAt 3 (nextRolls detState)

playTurn :: [Int] -> State -> State
playTurn rolls state =
  State
    { currentPlayer = nextPlayer state,
      nextPlayer = newNextPlayer
    }
  where
    newNextPlayer = (currentPlayer state) {score = newScore, pos = newPos}
    newScore = score (currentPlayer state) + newPos
    newPos = move (pos (currentPlayer state)) (sum rolls)
    move x d = ((x + d - 1) `mod` 10) + 1

main = do
  input <- readFile "inputs/Day21.txt"
  runTestTT $
    TestCase $ do
      solve1 (4, 8) @?= 739785
      solve1 (parse input) @?= 512442
      solve2 (4, 8) @?= 444356092776315
      solve2 (parse input) @?= 346642902541848
