module Day05 (main) where

import Control.Category ((>>>))
import qualified Data.Char as C
import Data.Function ((&))
import Data.Ix (inRange, range)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P

type Line = (V2 Int, V2 Int)

parse :: String -> [Line]
parse input = run $ do
  line `P.endBy1` eol <* P.eof
  where
    line = do
      from <- coord <* P.string " -> "
      to <- coord
      return (from, to)
    coord = do
      x <- number <* P.char ','
      y <- number
      return (V2 x y)
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: [Line] -> Int
solve1 input =
  input
    & filter isHOrV
    & fmap sort
    & concatMap range
    & countRepeatingPoints
  where
    isHOrV x = isHorizontal x || isVertical x
    isHorizontal (V2 x1 _, V2 x2 _) = x1 == x2
    isVertical (V2 _ y1, V2 _ y2) = y1 == y2

solve2 :: [Line] -> Int
solve2 input =
  input
    & concatMap diagonalRange
    & countRepeatingPoints

countRepeatingPoints :: [V2 Int] -> Int
countRepeatingPoints =
  flip zip (repeat 1)
    >>> M.fromListWith (+)
    >>> M.filter (> 1)
    >>> M.size

diagonalRange :: Line -> [V2 Int]
diagonalRange (from@(V2 x1 y1), to@(V2 x2 y2)) =
  from
    & iterate (+ step)
    & takeWhile (inRange bounds)
  where
    bounds = (V2 (min x1 x2) (min y1 y2), V2 (max x1 x2) (max y1 y2))
    step = V2 (deltaX `div` deltaGcd) (deltaY `div` deltaGcd)
    deltaGcd = gcd (abs deltaX) (abs deltaY)
    (V2 deltaX deltaY) = to - from

sort :: Line -> Line
sort (from, to)
  | from < to = (from, to)
  | otherwise = (to, from)

main = do
  input <- readFile "inputs/Day05.txt"
  exampleInput <- readFile "inputs/Day05_example.txt"
  runTestTT $
    TestCase $ do
      range (V2 950 698, V2 597 698) @?= []
      (range (V2 597 698, V2 950 698) /= []) @?= True
      (V2 597 698 < V2 950 698) @?= True
      diagonalRange (V2 1 1, V2 3 3) @?= [V2 1 1, V2 2 2, V2 3 3]
      diagonalRange (sort (V2 9 7, V2 7 9)) @?= [V2 7 9, V2 8 8, V2 9 7]
      diagonalRange (V2 9 7, V2 7 9) @?= [V2 9 7, V2 8 8, V2 7 9]
      diagonalRange (V2 9 7, V2 3 9) @?= [V2 9 7, V2 6 8, V2 3 9]
      solve1 (parse input) @?= 7269
      solve2 (parse exampleInput) @?= 12
      solve2 (parse input) @?= 21140
