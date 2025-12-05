module Day05 (main) where

import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Ix (inRange, rangeSize)
import Data.List qualified as L
import Data.Maybe (fromJust)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

type Range = (Int, Int)

parse :: String -> ([Range], [Int])
parse input = run $ do
  ranges <- range `P.endBy1` eol <* eol
  numbers <- number `P.endBy1` eol
  return (ranges, numbers)
  where
    -- Standard parsers
    range = do
      a <- number <* P.char '-'
      b <- number
      return (a, b)
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: ([Range], [Int]) -> Int
solve1 (ranges, ids) =
  ids
    & filter (\id -> any (`inRange` id) ranges)
    & length

solve2 :: ([Range], [Int]) -> Int
solve2 (ranges, _) =
  ranges
    & L.sortOn fst
    & foldl' go []
    <&> rangeSize
    & sum
  where
    go [] r = [r]
    go (prev@(prevStart, prevEnd) : rest) r@(start, end)
      | start <= prevEnd = (prevStart, max end prevEnd) : rest
      | otherwise = r : prev : rest

main = do
  input <- readFile "inputs/Day05.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 848
      solve2 (parse input) @?= 334714395325710
