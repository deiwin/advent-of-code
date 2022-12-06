module Day06 (main) where

import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.List as L
import Data.Maybe ( fromJust)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> String
parse = head . lines

solve1 :: String -> Int
solve1 input =
  L.zip5 [4..] input (drop 1 input) (drop 2 input) (drop 3 input)
    <&> toList
    & filter (unique . snd)
    & head
    & fst
  where
    toList (i, a, b, c, d) = (i, [a, b, c, d])

unique :: Eq a => [a] -> Bool
unique xs = length xs == length (L.nub xs)

solve2 :: String -> Int
solve2 input =
  input
    & L.tails
    <&> take 14
    & zip [14..]
    & L.find (unique . snd)
    & fromJust
    & fst

main = do
  input <- readFile "inputs/Day06.txt"
  runTestTT $
    TestCase $ do
      solve1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" @?= 7
      solve1 (parse input) @?= 1723
      solve2 (parse input) @?= 3708
