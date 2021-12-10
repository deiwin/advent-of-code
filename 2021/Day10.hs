module Day10 (main) where

import Control.Monad (foldM)
import Data.Either (lefts, rights)
import Data.Function ((&))
import Data.List (foldl')
import qualified Data.List as L
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [String]
parse = lines

solve1 :: [String] -> Int
solve1 input =
  input
    & fmap execute
    & lefts
    & fmap toScore
    & sum
  where
    toScore = \case
      ']' -> 57
      ')' -> 3
      '}' -> 1197
      '>' -> 25137
      _ -> undefined

execute :: String -> Either Char String
execute = foldM go ""
  where
    go :: String -> Char -> Either Char String
    go [] c
      | isOpenChar c = Right [c]
      | otherwise = Left c
    go s@(first : rest) c
      | isOpenChar c = Right (c : s)
      | first == toOpenChar c = Right rest
      | otherwise = Left c
    isOpenChar c = c `elem` ['[', '(', '{', '<']
    toOpenChar = \case
      ']' -> '['
      ')' -> '('
      '}' -> '{'
      '>' -> '<'
      _ -> undefined

solve2 :: [String] -> Int
solve2 input =
  input
    & fmap execute
    & rights
    & fmap (score . fmap toScore)
    & L.sort
    & middle
  where
    middle l = l !! (length l `div` 2)
    score = foldl' score' 0
    score' acc x = (5 * acc) + x
    toScore = \case
      '(' -> 1
      '[' -> 2
      '{' -> 3
      '<' -> 4
      _ -> undefined

main = do
  input <- readFile "inputs/Day10.txt"
  exampleInput <- readFile "inputs/Day10_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 26397
      solve1 (parse input) @?= 392139
      solve2 (parse exampleInput) @?= 288957
      solve2 (parse input) @?= 4001832844
