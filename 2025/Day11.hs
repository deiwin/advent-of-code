module Day11 (main) where

import Control.Monad.Memo (MonadMemo, memo, startEvalMemo)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [(String, [String])]
parse = fmap (f . words) . lines
  where
    f (x : xs) = (init x, xs)

pathCount :: HashMap String [String] -> String -> String -> Int
pathCount g from to = startEvalMemo $ go (from, to)
  where
    go :: (MonadMemo (String, String) Int m) => (String, String) -> m Int
    go (from, to)
      | from == to = return 1
      | otherwise =
          from
            & (g HM.!)
            & traverse (\next -> memo go (next, to))
            <&> sum

solve1 :: [(String, [String])] -> Int
solve1 input = pathCount (HM.fromList input) "you" "out"

data Res = Res
  { both :: Int,
    onlyA :: Int,
    onlyB :: Int,
    neither :: Int
  }

pathCount' :: HashMap String [String] -> (String, String) -> String -> String -> Res
pathCount' g (reqA, reqB) from to = startEvalMemo $ go (from, to)
  where
    go :: (MonadMemo (String, String) Res m) => (String, String) -> m Res
    go (from, to)
      | from == to = return (Res 0 0 0 1)
      | otherwise =
          from
            & (g HM.!)
            & traverse (\next -> memo go (next, to))
            <&> foldl' (merge from) (Res 0 0 0 0)
    merge from acc x
      | from == reqA =
          Res
            (acc.both + x.both + x.onlyB)
            (acc.onlyA + x.onlyA + x.neither)
            acc.onlyB
            acc.neither
      | from == reqB =
          Res
            (acc.both + x.both + x.onlyA)
            acc.onlyA
            (acc.onlyB + x.onlyB + x.neither)
            acc.neither
      | otherwise =
          Res
            (acc.both + x.both)
            (acc.onlyA + x.onlyA)
            (acc.onlyB + x.onlyB)
            (acc.neither + x.neither)

solve2 :: [(String, [String])] -> Int
solve2 input = both $ pathCount' (HM.fromList input) ("dac", "fft") "svr" "out"

main = do
  input <- readFile "inputs/Day11.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 724
      solve2 (parse input) @?= 473930047491888
