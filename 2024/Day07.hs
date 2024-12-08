module Day07 (main) where

import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl')
import Data.List qualified as L
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

parse :: String -> [(Int, [Int])]
parse input = run ((calibration `P.endBy1` eol) <* P.eof)
  where
    -- Standard parsers
    calibration = do
      result <- number <* P.char ':' <* spaces
      numbers <- number `P.sepBy1` spaces
      return (result, numbers)
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    spaces = P.many1 (P.char ' ')
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

valid :: [Int -> Int -> Int] -> (Int, [Int]) -> Bool
valid _ (_, []) = undefined
valid ops (result, x : xs) =
  foldl' f (S.singleton x) xs
    & S.member result
  where
    f acc x =
      acc
        & foldl' (g x) S.empty
    g x acc accV =
      ops
        <&> (\op -> accV `op` x)
        & filter (<= result)
        & S.fromList
        & S.union acc

solve1 :: [(Int, [Int])] -> Int
solve1 input =
  input
    & filter (valid [(+), (*)])
    <&> fst
    & sum

conOp :: Int -> Int -> Int
conOp a b =
  b + 1
    & fromIntegral
    & logBase 10
    & ceiling
    & fromIntegral
    & (10 ^)
    & (* a)
    & (+ b)

solve2 :: [(Int, [Int])] -> Int
solve2 input =
  input
    & filter (valid [(+), (*), conOp])
    <&> fst
    & sum

main = do
  input <- readFile "inputs/Day07.txt"
  exampleInput <- readFile "inputs/Day07_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 12940396350192
      solve2 (parse exampleInput) @?= 11387
      solve2 (parse input) @?= 106016735664498
