module Day04 (main) where

import Data.Char qualified as C
import Data.Function ((&))
import Data.List qualified as L
import Data.Maybe (fromJust)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

type Range = (Int, Int)

type Pair = (Range, Range)

parse :: String -> [Pair]
parse input = run $ line `P.endBy1` eol
  where
    line = do
      x <- range <* P.char ','
      y <- range
      return (x, y)
    range = do
      x <- number <* P.char '-'
      y <- number
      return (x, y)
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

contains :: Pair -> Bool
contains ((x, y), (z, w)) =
  z >= x && w <= y
    || x >= z && y <= w

overlap :: Pair -> Bool
overlap pair@((x, y), (z, w)) =
  x >= z && x <= w
    || y >= z && y <= w
    || contains pair

solve1 :: [Pair] -> Int
solve1 input =
  input
    & filter contains
    & length

solve2 :: [Pair] -> Int
solve2 input =
  input
    & filter overlap
    & length

main = do
  input <- readFile "inputs/Day04.txt"
  exampleInput <- readFile "inputs/Day04_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 459
      solve1 (parse exampleInput) @?= 2
      solve2 (parse input) @?= 779
      solve2 (parse exampleInput) @?= 4
