{-# LANGUAGE FlexibleContexts #-}

module Day25 (main) where

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import Data.Function ((&))
import Data.Ix (inRange, range)
import qualified Data.List as L
import Data.Maybe (fromJust)
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P

data Cell = Open | South | East
  deriving (Eq, Show, Ord)

type Grid = Array (V2 Int) Cell

parse :: String -> [[Cell]]
parse input = run (line `P.endBy1` eol <* P.eof)
  where
    line = P.many1 cell
    cell =
      Open <$ P.char '.'
        <|> South <$ P.char 'v'
        <|> East <$ P.char '>'
    -- Standard parsers
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: [[Cell]] -> Int
solve1 input =
  input
    & buildGrid
    & L.unfoldr (fmap (\x -> (x, x)) . step)
    & zip [2 ..] -- Not quite sure why starting from 2 but it works :shrug:
    & last
    & fst

step :: Grid -> Maybe Grid
step grid =
  case moveEast grid of
    Nothing -> moveSouth grid
    Just newGrid ->
      case moveSouth newGrid of
        Nothing -> Just newGrid
        Just newerGrid -> Just newerGrid
  where
    moveEast = move East (V2 0 1)
    moveSouth = move South (V2 1 0)
    move cell dir grid
      | L.null updates = Nothing
      | otherwise = Just (grid A.// updates)
      where
        updates =
          (second (const Open) <$> cellsToMove)
            ++ (first newPos <$> cellsToMove)
        cellsToMove = filter (isOpen . newPos . fst) cellsToConsider
        isOpen pos = grid A.! pos == Open
        newPos pos@(V2 y x)
          | inRange (A.bounds grid) (pos + dir) = pos + dir
          | dir == V2 0 1 = V2 y 0
          | otherwise = V2 0 x
        cellsToConsider = filter ((== cell) . snd) (A.assocs grid)

buildGrid :: [[Cell]] -> Grid
buildGrid input = A.array bounds cells
  where
    cells = zip (range bounds) (concat input)
    bounds = (V2 0 0, V2 (length input - 1) (length (head input) - 1))

main = do
  input <- readFile "inputs/Day25.txt"
  exampleInput <- readFile "inputs/Day25_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 58
      solve1 (parse input) @?= 530
