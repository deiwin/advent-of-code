module Day02 (main) where

import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Ix (inRange, range)
import Data.List qualified as L
import Data.Maybe (fromJust, fromMaybe)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

parse :: String -> [(Int, Int)]
parse input = run (range `P.sepBy1` P.char ',' <* eol)
  where
    range = do
      from <- number <* P.char '-'
      to <- number
      return (from, to)
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

digitCount :: Int -> Int
digitCount x
  | x >= 0 && x < 10 = 1
  | otherwise = 1 + digitCount (x `div` 10)

isRepeating :: Int -> Bool
isRepeating x
  | odd (digitCount x) = False
  | otherwise = firstHalf == secondHalf
  where
    halfSize = digitCount x `div` 2
    (firstHalf, secondHalf) = x `divMod` (10 ^ halfSize)

nextRepeating :: Int -> Int
nextRepeating x
  | odd size = 10 ^ size + (10 ^ (size `div` 2))
  | secondHalf < firstHalf = (firstHalf * (10 ^ halfSize)) + firstHalf
  | otherwise = ((firstHalf + 1) * (10 ^ (digitCount (firstHalf + 1)))) + firstHalf + 1
  where
    size = digitCount x
    halfSize = size `div` 2
    (firstHalf, secondHalf) = x `divMod` (10 ^ halfSize)

repeaters :: (Int, Int) -> [Int]
repeaters range@(from, to) = takeWhile (inRange range) $ iterate nextRepeating start
  where
    start
      | isRepeating from = from
      | otherwise = nextRepeating from

solve1 :: [(Int, Int)] -> Int
solve1 input =
  input
    & concatMap repeaters
    & sum

parts :: Int -> Int -> [Int]
parts partCount x = go [] x
  where
    go ps = \case
      0 -> ps
      x -> let (d, m) = x `divMod` (10 ^ partSize) in go (m : ps) d
    size = digitCount x
    partSize = size `div` partCount

isRepeating' :: Int -> Bool
isRepeating' x =
  [2 .. size]
    & filter ((== 0) . (size `mod`))
    & any (allEqual . (`parts` x))
  where
    size = digitCount x
    allEqual (x : xs) = all (== (x)) xs
    allEqual _ = undefined

solve2 :: [(Int, Int)] -> Int
solve2 input =
  input
    & concatMap (filter isRepeating' . range)
    & sum

main = do
  input <- readFile "inputs/Day02.txt"
  runTestTT $
    TestCase $ do
      (digitCount <$> [1, 23, 456]) @?= [1, 2, 3]
      (isRepeating <$> [1, 11, 12, 123123, 1231234]) @?= [False, True, False, True, False]
      (nextRepeating <$> [1, 11, 12, 21, 99, 123, 123123, 1231234])
        @?= [11, 22, 22, 22, 1010, 1010, 124124, 10001000]
      repeaters (1, 1234) @?= [11, 22, 33, 44, 55, 66, 77, 88, 99, 1010, 1111, 1212]
      solve1 (parse input) @?= 12599655151
      (isRepeating' <$> [1, 11, 12, 123123, 1231234, 123123123])
        @?= [False, True, False, True, False, True]
      solve2 (parse input) @?= 20942028255
