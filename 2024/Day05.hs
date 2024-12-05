module Day05 (main) where

import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

parse :: String -> ([(Int, Int)], [[Int]])
parse input = run $ do
  pairs <- pair `P.endBy1` eol <* eol
  rows <- (number `P.sepBy1` P.char ',') `P.endBy1` eol <* P.eof
  return (pairs, rows)
  where
    -- Standard parsers
    pair = do
      a <- number <* P.char '|'
      b <- number
      return (a, b)
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

buildPartialOrderMap :: (Foldable t, Ord a) => t (a, a) -> Map (a, a) Ordering
buildPartialOrderMap pairs =
  pairs
    & concatMap (\(a, b) -> [((a, b), LT), ((b, a), GT)])
    & M.fromList

isValid :: (Ord a) => Map (a, a) Ordering -> [a] -> Bool
isValid partialOrderMap update = L.sortBy (curry (partialOrderMap M.!)) update == update

middleElem :: [a] -> a
middleElem xs = xs !! (length xs `div` 2)

solve1 :: ([(Int, Int)], [[Int]]) -> Int
solve1 input =
  updates
    & filter (isValid partialOrderMap)
    <&> middleElem
    & sum
  where
    (pairs, updates) = input
    partialOrderMap = buildPartialOrderMap pairs

solve2 :: ([(Int, Int)], [[Int]]) -> Int
solve2 input =
  updates
    & filter (not . isValid partialOrderMap)
    <&> (middleElem . L.sortBy (curry (partialOrderMap M.!)))
    & sum
  where
    (pairs, updates) = input
    partialOrderMap = buildPartialOrderMap pairs

main = do
  input <- readFile "inputs/Day05.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 6384
      solve2 (parse input) @?= 5353
