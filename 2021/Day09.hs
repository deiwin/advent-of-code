module Day09 (main) where

import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import qualified Data.Char as C
import Data.Function ((&))
import Data.Ix (inRange, range)
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P

parse :: String -> [[Int]]
parse input = run $ P.many1 digit `P.endBy1` eol <* P.eof
  where
    -- Standard parsers
    digit :: P.ReadP Int
    digit = read . (: []) <$> P.satisfy C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: [[Int]] -> Int
solve1 input =
  range bounds
    & filter isLowPoint
    & fmap ((+ 1) . (array A.!))
    & sum
  where
    isLowPoint coord =
      surrounding coord
        & filter (inRange bounds)
        & all ((> (array A.! coord)) . (array A.!))
    surrounding coord = (+ coord) <$> [V2 (-1) 0, V2 1 0, V2 0 (-1), V2 0 1]
    array :: Array (V2 Int) Int
    array = A.listArray bounds (concat input)
    bounds = (V2 0 0, V2 (length input - 1) (length (head input) - 1))

solve2 :: [[Int]] -> Int
solve2 input =
  range bounds
    & filter isLowPoint
    & fmap (length . expand)
    & L.sort
    & reverse
    & take 3
    & product
  where
    expand coord = go (S.singleton coord) (linkedSurrounding coord)
      where
        go :: Set (V2 Int) -> [(V2 Int, V2 Int)] -> [V2 Int]
        go visited [] = S.toList visited
        go visited toVisit = go allNewVisited newToVisit
          where
            newToVisit =
              newVisited
                & concatMap linkedSurrounding
                & filter matching
            allNewVisited = visited `S.union` S.fromList newVisited
            newVisited =
              toVisit
                & filter matching
                & fmap snd
            matching (from, to)
              | to `S.member` visited = False
              | (array A.! to) < (array A.! from) = False
              | (array A.! to) == 9 = False
              | otherwise = True
        linkedSurrounding coord = zip (repeat coord) (surrounding coord)
    isLowPoint coord =
      surrounding coord
        & all ((> (array A.! coord)) . (array A.!))
    surrounding coord =
      [V2 (-1) 0, V2 1 0, V2 0 (-1), V2 0 1]
        & fmap (+ coord)
        & filter (inRange bounds)
    array :: Array (V2 Int) Int
    array = A.listArray bounds (concat input)
    bounds = (V2 0 0, V2 (length input - 1) (length (head input) - 1))

main = do
  input <- readFile "inputs/Day09.txt"
  exampleInput <- readFile "inputs/Day09_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 15
      solve1 (parse input) @?= 502
      solve2 (parse exampleInput) @?= 1134
      solve2 (parse input) @?= 1330560
