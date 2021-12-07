module Day07 (main) where

import Data.Text (Text, splitOn, unpack)
import Data.Text.IO (readFile)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Prelude hiding (readFile)

parse :: Text -> [Int]
parse input = read . unpack <$> splitOn "," input

solve1 :: [Int] -> Int
solve1 input = minimum (flip cost input <$> range)
  where
    cost midPoint = sum . fmap (abs . (midPoint -))
    range = [(minimum input) .. (maximum input)]

solve2 :: [Int] -> Int
solve2 input = minimum (flip cost input <$> range)
  where
    cost midPoint = sum . fmap (newCost . abs . (midPoint -))
    newCost x = ((1 + x) * x) `div` 2
    range = [(minimum input) .. (maximum input)]

main = do
  input <- readFile "inputs/Day07.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 344297
      solve2 (parse input) @?= 97164301
