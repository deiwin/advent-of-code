module Day09 (main) where

import Control.Applicative ((<|>))
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Ix (inRange)
import Data.List qualified as L
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P
import Prelude hiding (Either (..))

data Direction = Up | Down | Left | Right
  deriving (Show)

data Move = Move
  { direction :: Direction,
    count :: Int
  }
  deriving (Show)

type Coord = V2 Int

data State = State
  { head :: Coord,
    tail :: Coord
  }
  deriving (Show, Eq)

parse :: String -> [Move]
parse input = run $ move `P.endBy` eol
  where
    move = do
      direction <- direction <* P.char ' '
      count <- number
      return $ Move {..}
    direction =
      Up <$ P.char 'U'
        <|> Down <$ P.char 'D'
        <|> Left <$ P.char 'L'
        <|> Right <$ P.char 'R'
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

initialState :: State
initialState = State {..}
  where
    head = V2 0 0
    tail = V2 0 0

directionDiff :: Direction -> V2 Int
directionDiff = \case
  Up -> V2 (-1) 0
  Down -> V2 1 0
  Left -> V2 0 (-1)
  Right -> V2 0 1

move :: State -> V2 Int -> State
move s diff =
  State
    { head = s.head + diff,
      tail = s.tail + tailMove
    }
  where
    tailDiff = s.head - s.tail
    follow = \case
      (V2 0 x) -> V2 0 (x `div` abs x)
      (V2 y 0) -> V2 (y `div` abs y) 0
      (V2 y x) -> V2 (y `div` abs y) (x `div` abs x)
    tailMove
      | inRange (V2 (-1) (-1), V2 1 1) (diff + tailDiff) = V2 0 0
      | otherwise = follow (diff + tailDiff)

solve1 :: [Move] -> Int
solve1 input =
  input
    & concatMap (\m -> replicate m.count m.direction)
    & L.scanl' (\s dir -> move s (directionDiff dir)) initialState
    <&> (\s -> s.tail)
    & S.fromList
    & S.size

data StateN = StateN
  { head :: Coord,
    tail :: [Coord]
  }
  deriving (Show, Eq)

move' :: StateN -> Direction -> StateN
move' s dir =
  StateN
    { head = s.head + directionDiff dir,
      tail = newTail
    }
  where
    newTail =
      zip (s.head : s.tail) s.tail
        & L.mapAccumL f (directionDiff dir)
        & snd
    f diff (h, t) = (newDiff, newState.tail)
      where
        newDiff = newState.tail - t
        newState = move (State {head = h, tail = t}) diff

initialState9 :: StateN
initialState9 = StateN {head = V2 0 0, tail = replicate 9 (V2 0 0)}

moveAll' :: StateN -> [Move] -> [StateN]
moveAll' s ms =
  ms
    & concatMap (\m -> replicate m.count m.direction)
    & L.scanl' move' s

solve2 :: [Move] -> Int
solve2 input =
  moveAll' initialState9 input
    <&> (\s -> last s.tail)
    & S.fromList
    & S.size

main = do
  input <- readFile "inputs/Day09.txt"
  exampleInput <- readFile "inputs/Day09_example.txt"
  exampleInput2 <- readFile "inputs/Day09_example2.txt"
  runTestTT $
    TestCase $ do
      move (State {head = V2 0 0, tail = V2 0 0}) (directionDiff Right) @?= (State {head = V2 0 1, tail = V2 0 0})
      move (State {head = V2 0 1, tail = V2 0 0}) (directionDiff Right) @?= (State {head = V2 0 2, tail = V2 0 1})
      move (State {head = V2 0 1, tail = V2 0 0}) (directionDiff Down) @?= (State {head = V2 1 1, tail = V2 0 0})
      move (State {head = V2 1 1, tail = V2 0 0}) (directionDiff Down) @?= (State {head = V2 2 1, tail = V2 1 1})
      solve1 (parse exampleInput) @?= 13
      solve1 (parse input) @?= 6284
      last (moveAll' initialState9 [Move Right 5, Move Down 8, Move Left 4])
        @?= StateN
          { head = V2 8 1,
            tail =
              [ V2 8 2,
                V2 8 3,
                V2 7 3,
                V2 6 3,
                V2 5 3,
                V2 4 3,
                V2 3 3,
                V2 2 2,
                V2 1 1
              ]
          }
      solve2 (parse exampleInput) @?= 1
      solve2 (parse exampleInput2) @?= 36
      solve2 (parse input) @?= 2661
