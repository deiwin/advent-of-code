module Day01 (main) where

import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Prelude hiding (readFile)

parse :: Text -> [Int]
parse input = read . T.unpack <$> T.lines input

solve :: [Int] -> Int
solve = countIncreases

countIncreases :: [Int] -> Int
countIncreases xs =
  xs
    & zipWith (-) (tail xs)
    & filter (> 0)
    & length

solve2 :: [Int] -> Int
solve2 input = countIncreases sum
  where
    sum = zipWith3 (\a b c -> a + b + c) input (tail input) (tail (tail input))

main = do
  input <- readFile "inputs/Day01.txt"
  runTestTT $
    TestCase $ do
      solve (parse input) @?= 1527
      solve2 (parse input) @?= 1575
