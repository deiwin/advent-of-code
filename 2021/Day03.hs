module Day03 (main) where

import Control.Category ((>>>))
import Control.Monad (foldM)
import qualified Data.Bits as B
import Data.Function ((&))
import Data.List (foldl')
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Void (Void)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Prelude hiding (readFile)

type Bit = Bool

type CommonBit = Maybe Bit

parse :: Text -> [[Bit]]
parse input = run parser
  where
    parser = P.some bit `P.sepEndBy1` P.space
    bit =
      False <$ P.char '0'
        P.<|> True <$ P.char '1'
    run p = case P.parse p "" input of
      Left bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
      Right result -> result

solve1 :: [[Bit]] -> Int
solve1 input = gamma * epsilon
  where
    gamma = fromBits (toCommonBit <$> counts)
    epsilon = fromBits (toLeastCommonBit <$> counts)
    toCommonBit (Just bit) = bit
    toCommonBit Nothing = undefined
    toLeastCommonBit (Just bit) = B.complement bit
    toLeastCommonBit Nothing = undefined
    counts = count input

solve2 :: [[Bit]] -> Int
solve2 input = oxygenGenRating * coScrubRating
  where
    oxygenGenRating = fromBits $ untilOne oxygenGenRatingF input
    oxygenGenRatingF bit (Just commonBit) = B.complement $ B.xor bit commonBit
    oxygenGenRatingF bit Nothing = bit
    coScrubRating = fromBits $ untilOne coScrubRatingF input
    coScrubRatingF bit (Just commonBit) = B.xor bit commonBit
    coScrubRatingF bit Nothing = B.complement bit
    untilOne :: (Bit -> CommonBit -> Bool) -> [[Bit]] -> [Bit]
    untilOne bitFilterF binaries =
      foldM go (count binaries, binaries) [0 ..]
        & either id undefined
      where
        go (_, [binary]) _ = Left binary
        go (counts, binaries) pos = Right (newCounts, newBinaries)
          where
            newCounts = count newBinaries
            newBinaries = filter binaryFilterF binaries
            binaryFilterF bits = bitFilterF (bits !! pos) (counts !! pos)

count :: [[Bit]] -> [CommonBit]
count input = toCommonBit <$> foldl' f (replicate width startState) input
  where
    toCommonBit (zeros, ones)
      | zeros > ones = Just False
      | zeros < ones = Just True
      | otherwise = Nothing
    f acc bits = zipWith g acc bits
    g (zeros, ones) True = (zeros, ones + 1)
    g (zeros, ones) False = (zeros + 1, ones)
    startState = (0, 0)
    width = length $ head input

fromBits :: [Bit] -> Int
fromBits =
  reverse
    >>> zip [0 ..]
    >>> filter snd -- Filter out all False (or 0) bits
    >>> fmap fst -- Keep the index (from [0..]) of the remaining True (or 1) bits
    >>> foldl' B.setBit 0

main = do
  input <- readFile "inputs/Day03.txt"
  exampleInput <- readFile "inputs/Day03_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 1092896
      solve2 (parse exampleInput) @?= 230
      solve2 (parse input) @?= 4672151
