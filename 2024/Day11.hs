module Day11 (main) where

import Control.Monad.Memo (MonadMemo, memo, startEvalMemo)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl')
import Data.List qualified as L
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [Int]
parse = fmap read . words . head . lines

digits :: Int -> [Int]
digits = reverse . f
  where
    f x
      | q == 0 = [r]
      | otherwise = r : f q
      where
        (q, r) = x `divMod` 10

unDigits :: [Int] -> Int
unDigits = foldl' (\acc x -> acc * 10 + x) 0

blink :: [Int] -> [Int]
blink = concatMap blink'

blink' :: Int -> [Int]
blink' 0 = [1]
blink' n
  | even digitCount = unDigits <$> [h1, h2]
  | otherwise = [n * 2024]
  where
    (h1, h2) = L.splitAt (digitCount `div` 2) nDigits
    nDigits = digits n
    digitCount = length nDigits

solve1 :: [Int] -> Int
solve1 input =
  input
    & iterate blink
    & (!! 25)
    & length

stoneCount :: Int -> Int -> Int
stoneCount n x = startEvalMemo $ f (n, x)
  where
    f :: (MonadMemo (Int, Int) Int m) => (Int, Int) -> m Int
    f (0, _) = return 1
    f (n, x) = sum <$> traverse (\x' -> memo f (n - 1, x')) (blink' x)

solve2 :: [Int] -> Int
solve2 input =
  input
    <&> stoneCount 75
    & sum

main = do
  input <- readFile "inputs/Day11.txt"
  exampleInput <- readFile "inputs/Day11_example.txt"
  runTestTT $
    TestCase $ do
      digits 1234 @?= [1, 2, 3, 4]
      unDigits [1, 2, 3, 4] @?= 1234
      solve1 (parse exampleInput) @?= 55312
      solve1 (parse input) @?= 218079
      solve2 (parse input) @?= 259755538429618
