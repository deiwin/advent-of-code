module Day03 (main) where

import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.Maybe (fromJust)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

parse :: String -> [String]
parse input = run $ P.many1 letter `P.endBy1` eol
  where
    letter = P.satisfy C.isLetter
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: [String] -> Int
solve1 input =
  input
    <&> score . head . uncurry L.intersect . splitHalf
    & sum
  where
    splitHalf xs = splitAt (length xs `div` 2) xs

score :: Char -> Int
score c
  | C.isUpper c = C.ord c - C.ord 'A' + 27
  | otherwise = C.ord c - C.ord 'a' + 1

solve2 :: [String] -> Int
solve2 input =
  input
    & chunksOf 3
    <&> score . head . L.foldl1' L.intersect
    & sum
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

main = do
  input <- readFile "inputs/Day03.txt"
  exampleInput <- readFile "inputs/Day03_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 157
      solve1 (parse input) @?= 7737
      solve2 (parse exampleInput) @?= 70
      solve2 (parse input) @?= 2697
