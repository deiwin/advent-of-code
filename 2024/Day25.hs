module Day25 (main) where

import Data.Function ((&))
import Data.List qualified as L
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [[String]]
parse = filter f . L.groupBy grid . lines
  where
    grid a b = a /= "" && b /= ""
    f = \case
      [""] -> False
      _ -> True

cartProd :: [a] -> [a] -> [(a, a)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

solve1 :: [[String]] -> Int
solve1 input =
  cartProd keyCounts lockCounts
    & filter ((<= 5) . maximum . uncurry (zipWith (+)))
    & length
  where
    (keys, locks) = L.partition (all (== '.') . head) input
    lockCounts = fmap (length . takeWhile (== '#')) . L.transpose . tail <$> locks
    keyCounts = fmap (length . takeWhile (== '#') . reverse) . L.transpose . init <$> keys

main = do
  input <- readFile "inputs/Day25.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 3136
