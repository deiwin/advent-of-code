module Practice.Y2020D01 (main) where

import Control.Monad (guard)
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.Maybe ( fromJust)
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

solve1 :: [Int] -> Maybe Int
solve1 input =
  options
    & L.find ((== 2020) . uncurry (+))
    <&> uncurry (*)
  where
    options = do
      (i, x) <- zip [0 ..] input
      (j, y) <- zip [0 ..] input
      guard (i /= j)
      return (x, y)

solve2 :: [Int] -> Maybe Int
solve2 input =
  options
    & L.find ((== 2020) . (\(a, b, c) -> a + b + c))
    <&> (\(a, b, c) -> a * b * c)
  where
    options = do
      (i, x) <- zip [0 ..] input
      (j, y) <- zip [0 ..] input
      (k, z) <- zip [0 ..] input
      guard (i /= j)
      guard (i /= k)
      guard (j /= k)
      return (x, y, z)

main = do
  input <- readFile "Practice/inputs/Y2020D01.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= Just 926464
      solve2 (parse input) @?= Just 65656536
