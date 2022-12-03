module Day03 (main) where

import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [String]
parse = lines

solve1 :: [String] -> Int
solve1 input =
  input
    <&> score . head . uncurry L.intersect . splitHalf
    & sum
  where
    splitHalf xs = splitAt (length xs `div` 2) xs

score :: Char -> Int
score c
  | C.isUpper c = C.ord c - C.ord 'A' + 27
  | otherwise = C.ord c - C.ord 'a' + 1

solve2 :: [String] -> Int
solve2 input =
  input
    & chunksOf 3
    <&> score . head . L.foldl1' L.intersect
    & sum
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

main = do
  input <- readFile "inputs/Day03.txt"
  exampleInput <- readFile "inputs/Day03_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 157
      solve1 (parse input) @?= 7737
      solve2 (parse exampleInput) @?= 70
      solve2 (parse input) @?= 2697
