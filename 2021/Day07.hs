module Day07 (main) where

import Data.Text (Text, splitOn, unpack)
import Data.Text.IO (readFile)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Prelude hiding (readFile)

parse :: Text -> [Int]
parse input = read . unpack <$> splitOn "," input

solve1 :: [Int] -> Int
solve1 = bSearch cost
  where
    cost to = sum . fmap (abs . (to -))

solve2 :: [Int] -> Int
solve2 = bSearch cost
  where
    cost to = sum . fmap (newCost . abs . (to -))
    newCost x = ((1 + x) * x) `div` 2

bSearch :: (Int -> [Int] -> Int) -> [Int] -> Int
bSearch costF input = cost $ go (min, max)
  where
    (min, max) = (minimum input, maximum input)
    cost = flip costF input
    go (low, high)
      | low == high || high < low = low
      | otherwise = go (newLow, newHigh)
      where
        (newLow, newHigh) = case costs of
          [prev, this, next]
            | this < prev && this < next -> (mid, mid)
            | prev < this -> (low, mid)
            | otherwise -> (mid, high)
          _ -> undefined
        costs = cost . (+ mid) <$> [-1, 0, 1]
        mid = low + ((high - low) `div` 2)

main = do
  input <- readFile "inputs/Day07.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 344297
      solve2 (parse input) @?= 97164301
