module Day22 (main) where

import Data.Bits (shiftL, shiftR, xor)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (foldl1')
import Data.List qualified as L
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [Int]
parse = fmap read . lines

evolve :: Int -> Int
evolve x =
  x
    & op (`shiftL` 6)
    & op (`shiftR` 5)
    & op (`shiftL` 11)
  where
    op f x = mixAndPrune x (f x)
    mixAndPrune x y = prune $ mix x y
    mix = xor
    prune = (`rem` 16777216)

evolveN :: Int -> Int -> Int
evolveN n = (!! n) . iterate evolve

solve1 :: [Int] -> Int
solve1 input =
  input
    <&> evolveN 2000
    & sum

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (tail xs) xs

options :: [Int] -> HashMap (Int, Int, Int, Int) Int
options prices =
  prices
    & L.tails
    <&> take 5
    & takeWhile ((== 5) . length)
    <&> f
    & HM.fromListWith (\_new old -> old)
  where
    f :: [Int] -> ((Int, Int, Int, Int), Int)
    f xs = let [d1, d2, d3, d4] = diffs xs in ((d1, d2, d3, d4), last xs)

solve2 :: [Int] -> Int
solve2 input =
  input
    <&> (options . fmap (`rem` 10) . take 2001 . iterate evolve)
    & foldl1' (HM.unionWith (+))
    & HM.elems
    & maximum

main = do
  input <- readFile "inputs/Day22.txt"
  exampleInput <- readFile "inputs/Day22_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 37990510
      solve1 (parse input) @?= 16953639210
      solve2 (parse exampleInput) @?= 23
      solve2 (parse input) @?= 1863
