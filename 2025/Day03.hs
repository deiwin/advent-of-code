module Day03 (main) where

import Data.Containers.ListUtils (nubIntOn)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl1')
import Data.List qualified as L
import Data.Ord (comparing)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [[Int]]
parse = fmap (fmap (read . (: []))) . lines

undigits :: [Int] -> Int
undigits = foldl1' (\acc d -> (acc * 10) + d)

highest :: Int -> [Int] -> Int
highest n digits = undigits $ go n digits
  where
    go 1 digits = (: []) $ maximum digits
    go n digits =
      zip digits (tail (L.tails digits))
        & takeWhile ((== (n - 1)) . length . take (n - 1) . snd)
        & nubIntOn fst
        & L.maximumBy (comparing fst)
        & (\(d, rest) -> d : go (n - 1) rest)

solve1 :: [[Int]] -> Int
solve1 input =
  input
    <&> highest 2
    & sum

solve2 :: [[Int]] -> Int
solve2 input =
  input
    <&> highest 12
    & sum

main = do
  input <- readFile "inputs/Day03.txt"
  runTestTT $
    TestCase $ do
      undigits [1, 2, 3] @?= 123
      solve1 (parse input) @?= 17074
      solve2 (parse input) @?= 169512729575727
