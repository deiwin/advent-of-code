module Day06 (main) where

import Data.Function ((&))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Text (Text, splitOn, unpack)
import Data.Text.IO (readFile)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Prelude hiding (readFile)

parse :: Text -> [Int]
parse input = read . unpack <$> splitOn "," input

solve1 :: [Int] -> Int
solve1 = solveForDay 80

solve2 :: [Int] -> Int
solve2 = solveForDay 256

solveForDay :: Int -> [Int] -> Int
solveForDay day input =
  input
    & flip zip (repeat 1)
    & IM.fromListWith (+)
    & iterate playRound
    & (L.!! day)
    & IM.foldl' (+) 0

playRound :: IntMap Int -> IntMap Int
playRound state =
  state
    & IM.assocs
    & concatMap updateForAge
    & IM.fromListWith (+)
  where
    updateForAge (0, count) = [(6, count), (8, count)]
    updateForAge (age, count) = [(age - 1, count)]

main = do
  input <- readFile "inputs/Day06.txt"
  runTestTT $
    TestCase $ do
      solve1 [3, 4, 3, 1, 2] @?= 5934
      solve1 (parse input) @?= 388739
      solve2 (parse input) @?= 1741362314973
