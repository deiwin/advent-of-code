module Day02 (main) where

import Data.Function ((&))
import Data.List qualified as L
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [[Int]]
parse input = fmap read . words <$> lines input

safe :: [Int] -> Bool
safe xs =
  all f diffs || all (f . (* (-1))) diffs
  where
    diffs = zipWith (-) xs (tail xs)
    f x = x >= 1 && x <= 3

safe2 :: [Int] -> Bool
safe2 xs =
  zipWith (++) (L.inits xs) (tail $ L.tails xs)
    & any safe

solve1 :: [[Int]] -> Int
solve1 input =
  input
    & filter safe
    & length

solve2 :: [[Int]] -> Int
solve2 input =
  input
    & filter safe2
    & length

main = do
  input <- readFile "inputs/Day02.txt"
  exampleInput <- readFile "inputs/Day02_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 559
      solve2 (parse exampleInput) @?= 4
      solve2 (parse input) @?= 601
