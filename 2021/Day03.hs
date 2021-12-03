module Day03 (main) where

import Data.List (foldl')
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Void (Void)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Prelude hiding (readFile)

parse :: Text -> [[Char]]
parse input = run parser
  where
    parser = P.some P.digitChar `P.sepEndBy1` P.space
    run p = case P.parse p "" input of
      Left bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
      Right result -> result

solve1 :: [[Char]] -> Int
solve1 input = gamma * epsilon
  where
    gamma = binaryToInt (toCommonBit <$> counts)
    epsilon = binaryToInt (toLeastCommonBit <$> counts)
    toCommonBit (zeros, ones)
      | zeros > ones = 0
      | otherwise = 1
    toLeastCommonBit (zeros, ones)
      | zeros > ones = 1
      | otherwise = 0
    counts = count input

solve2 :: [[Char]] -> Int
solve2 input = oxygenGenRating * coScrubRating
  where
    oxygenGenRating = strToInt $ untilOne oxygenGenRatingF input
    oxygenGenRatingF '0' (zeros, ones)
      | zeros > ones = True
      | otherwise = False
    oxygenGenRatingF '1' (zeros, ones)
      | ones >= zeros = True
      | otherwise = False
    oxygenGenRatingF _ _ = undefined
    coScrubRating = strToInt $ untilOne coScrubRatingF input
    coScrubRatingF '0' (zeros, ones)
      | zeros <= ones = True
      | otherwise = False
    coScrubRatingF '1' (zeros, ones)
      | ones < zeros = True
      | otherwise = False
    coScrubRatingF _ _ = undefined
    untilOne :: (Char -> (Int, Int) -> Bool) -> [[Char]] -> [Char]
    untilOne f binaries = go 0 (count binaries) f binaries
      where
        go pos counts f binaries =
          if length result == 1
            then head result
            else go (pos + 1) (count result) f result
          where
            result = filter g binaries
            g bits = f (bits !! pos) (counts !! pos)

count :: [[Char]] -> [(Int, Int)]
count input = foldl' f (replicate width startState) input
  where
    f acc bits = zipWith g acc bits
    g (zeros, ones) '1' = (zeros, ones + 1)
    g (zeros, ones) '0' = (zeros + 1, ones)
    g _ _ = undefined
    startState = (0, 0)
    width = length $ head input

binaryToInt :: [Int] -> Int
binaryToInt xs = go $ reverse xs
  where
    go [] = 0
    go (x : xs) = x + 2 * go xs

strToInt :: [Char] -> Int
strToInt xs = binaryToInt (read . (: []) <$> xs)

main = do
  input <- readFile "inputs/Day03.txt"
  exampleInput <- readFile "inputs/Day03_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 1092896
      solve2 (parse exampleInput) @?= 230
      solve2 (parse input) @?= 4672151
