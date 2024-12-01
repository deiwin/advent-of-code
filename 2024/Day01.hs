module Day01 (main) where

import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Maybe (fromJust)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

parse :: String -> [(Int, Int)]
parse input = run $ do
  numbers <- pair `P.sepBy1` eol
  eol *> P.eof
  return numbers
  where
    pair = do
      a <- number <* spaces
      b <- number
      return (a, b)
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    spaces = P.many1 (P.char ' ')
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

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
