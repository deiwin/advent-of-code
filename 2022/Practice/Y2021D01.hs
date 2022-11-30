module Practice.Y2021D01 (main) where

import Data.Char qualified as C
import Data.Function ((&))
import Data.List qualified as L
import Data.Maybe (fromJust)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

parse :: String -> [Int]
parse input = run $ do
  numbers <- number `P.sepBy1` eol
  eol *> P.eof
  return numbers
  where
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: [Int] -> Int
solve1 input =
  input
    & flip zip (drop 1 input)
    & filter (uncurry (<))
    & length

solve2 :: [Int] -> Int
solve2 input =
  zip3 input (drop 1 input) (drop 2 input)
    & fmap (\(a, b, c) -> a + b + c)
    & solve1

main = do
  input <- readFile "Practice/inputs/Y2021D01.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 1527
      solve2 (parse input) @?= 1575
