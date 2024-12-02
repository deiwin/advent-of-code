module Day02 (main) where

import Data.Char qualified as C
import Data.Function ((&))
import Data.List qualified as L
import Data.Maybe (fromJust)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

parse :: String -> [[Int]]
parse input = run $ do
  numbers <- (number `P.sepBy1` spaces) `P.sepBy1` eol
  eol *> P.eof
  return numbers
  where
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    spaces = P.many1 (P.char ' ')
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

safe :: [Int] -> Bool
safe xs =
  all f diffs || all (f . (* (-1))) diffs
  where
    diffs = zipWith (-) xs (tail xs)
    f x = x >= 1 && x <= 3

safe2 :: [Int] -> Bool
safe2 xs =
  zipWith (++) (L.inits xs) (tail $ L.tails xs)
    & any safe

solve1 :: [[Int]] -> Int
solve1 input =
  input
    & filter safe
    & length

solve2 :: [[Int]] -> Int
solve2 input =
  input
    & filter safe2
    & length

main = do
  input <- readFile "inputs/Day02.txt"
  exampleInput <- readFile "inputs/Day02_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 559
      solve2 (parse exampleInput) @?= 4
      solve2 (parse input) @?= 601
