module Day01 (main) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [String]
parse = lines

toI :: String -> Int
toI = \case
  ('L' : rest) -> read ('-' : rest)
  ('R' : rest) -> read rest
  _ -> undefined

solve1 :: [String] -> _
solve1 input =
  input
    <&> toI
    & L.scanl (\acc x -> (acc + x) `mod` 100) 50
    & filter (== 0)
    & length

solve2 :: [String] -> _
solve2 input =
  input
    <&> toI
    & L.mapAccumL go 50
    & snd
    & sum
  where
    go acc x =
      let (d, newAcc) = (acc + x) `divMod` 100
          zeroCount
            | acc /= 0 && newAcc == 0 = if d <= 0 then abs d + 1 else d
            | acc == 0 && d < 0 = if newAcc == 0 then abs d else abs d - 1
            | otherwise = abs d
       in (newAcc, zeroCount)

main = do
  input <- readFile "inputs/Day01.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 1084
      solve2 (parse "L50\nL100") @?= 2
      solve2 (parse "L50\nL101") @?= 2
      solve2 (parse "R50") @?= 1
      solve2 (parse "R51") @?= 1
      solve2 (parse input) @?= 6475
