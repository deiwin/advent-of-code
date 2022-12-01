module Day01 (main) where

import Data.Char qualified as C
import Data.Function ((&))
import Data.List qualified as L
import Data.Maybe (fromJust)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

parse :: String -> [[Int]]
parse input = run $ do
  elves <- calories `P.sepBy` (eol *> eol)
  eol *> P.eof
  return elves
  where
    -- Standard parsers
    calories = number `P.sepBy1` eol
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: [[Int]] -> Int
solve1 input =
  input
    & fmap sum
    & maximum

solve2 :: [[Int]] -> Int
solve2 input =
  input
    & fmap sum
    & L.sort
    & reverse
    & take 3
    & sum

main = do
  input <- readFile "inputs/Day01.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 71300
      solve2 (parse input) @?= 209691
