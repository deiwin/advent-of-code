module Day06 (main) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

solve1 :: String -> Int
solve1 input =
  input
    & lines
    <&> words
    & L.transpose
    <&> (calc . reverse)
    & sum
  where
    calc = \case
      ("+" : rest) -> sum (read <$> rest)
      ("*" : rest) -> product (read <$> rest)
      _ -> undefined

trim :: String -> String
trim = takeWhile (/= ' ') . dropWhile (== ' ')

solve2 :: String -> Int
solve2 input =
  input
    & lines
    & L.transpose
    & foldl' go (undefined, [], [])
    & (\(f, xs, res) -> f xs : res)
    & sum
  where
    go :: ([Int] -> Int, [Int], [Int]) -> String -> ([Int] -> Int, [Int], [Int])
    go (f, xs, res) s
      | null (trim s) = (newF, [], f xs : res)
      | otherwise = (newF, x : xs, res)
      where
        x = read $ trim $ init s
        newF =
          case last s of
            '+' -> sum
            '*' -> product
            ' ' -> f
            _ -> undefined

main = do
  input <- readFile "inputs/Day06.txt"
  exampleInput <- readFile "inputs/Day06_example.txt"
  runTestTT $
    TestCase $ do
      solve1 input @?= 3968933219902
      solve2 exampleInput @?= 3263827
      solve2 input @?= 6019576291014
