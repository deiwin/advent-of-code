module Day03 (main) where

import Control.Applicative (empty)
import Control.Arrow (second, (>>>))
import Control.Monad (guard)
import Criterion.Main
  ( bench,
    defaultMain,
    whnf,
  )
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import Data.Function ((&))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Ix
  ( inRange,
    range,
  )
import Data.List
  ( foldl',
    foldl1',
    isPrefixOf,
    iterate,
  )
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
  ( catMaybes,
    isJust,
  )
import Data.Ord (comparing)
import Data.Sequence
  ( Seq (..),
    (<|),
    (|>),
  )
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Data.Void (Void)
import Debug.Trace
  ( traceShow,
    traceShowId,
  )
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import Test.HUnit.Base
  ( Test (TestCase),
    (@?=),
  )
import Test.HUnit.Text (runTestTT)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PL
import Prelude hiding (readFile)

type Parser = P.Parsec Void Text

parse :: Text -> _
parse input = run parser
  where
    parser = P.some P.digitChar `P.sepEndBy1` P.space
    run p = case P.parse p "" input of
      Left bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
      Right result -> result

solve1 :: _
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
    counts = foldl' f (replicate width startState) input
      where
        f acc bits = zipWith g acc bits
        g (zeros, ones) '1' = (zeros, ones + 1)
        g (zeros, ones) '0' = (zeros + 1, ones)
        g _ _ = undefined
        startState = (0, 0)
    width = length $ head input

solve2 :: _
solve2 input = oxygenGenRating * coScrubRating
  where
    oxygenGenRating = strToInt $ untilOne oxygenGenRatingF input
    -- oxygenGenRatingF b c | traceShow (b, c) False = undefined
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
    untilOne = go 0
      where
        -- go pos _ i | traceShow (pos, i) False = undefined
        go pos f binaries =
          if length result == 1
            then head result
            else go (pos + 1) f result
          where
            result = filter g binaries
            g bits = f (bits !! pos) (counts binaries !! pos)
    counts input = foldl' f (replicate width startState) input
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
  print $ solve2 $ parse input
  -- print $ solve2 $ parse exampleInput
  runTestTT $
    TestCase $ do
      1 @?= 2
      1 @?= 1
