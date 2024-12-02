module Day01 (main) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IntMap qualified as IM
import Data.List qualified as L
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [(Int, Int)]
parse input = (\[a, b] -> (a, b)) . fmap read . words <$> lines input

solve1 :: [(Int, Int)] -> Int
solve1 input =
  zipWith (\a b -> abs (a - b)) as bs
    & sum
  where
    as = L.sort $ fst <$> input
    bs = L.sort $ snd <$> input

solve2 :: [(Int, Int)] -> Int
solve2 input =
  sum (as <&> \a -> a * IM.findWithDefault 0 a bs)
  where
    as = L.sort $ fst <$> input
    bs =
      input
        <&> (,1) . snd
        & IM.fromListWith (+)

main = do
  input <- readFile "inputs/Day01.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 1830467
      solve2 (parse input) @?= 26674158
