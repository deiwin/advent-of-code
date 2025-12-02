module Day02 (main) where

import Control.Applicative (empty, (<|>))
import Control.Arrow (first, second, (>>>))
import Control.Monad (guard)
import Criterion.Main
  ( bench,
    defaultMain,
    whnf,
  )
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import qualified Data.Char as C
import Data.Function ((&))
import Data.Functor ((<$), (<&>), ($>))
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
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Maybe
  ( catMaybes,
    fromJust,
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
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import Data.Void (Void)
import Debug.Trace
  ( traceShow,
    traceShowId,
  )
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P
import Control.Monad.Combinators qualified as PC

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

solve2 :: _
solve2 input =
  input

main = do
  input <- readFile "inputs/Day02.txt"
  -- exampleInput <- readFile "inputs/Day02_example.txt"
  print $ solve1 $ parse input
  -- print $ solve2 $ parse input
  runTestTT $
    TestCase $ do
      (digitCount <$> [1, 23, 456]) @?= [1, 2, 3]
      (isRepeating <$> [1, 11, 12, 123123, 1231234]) @?= [False, True, False, True, False]
      (nextRepeating <$> [1, 11, 12, 21, 99, 123, 123123, 1231234])
        @?= [11, 22, 22, 22, 1010, 1010, 124124, 10001000]
      repeaters (1, 1234) @?= [11, 22, 33, 44, 55, 66, 77 ,88, 99, 1010, 1111, 1212]
      solve1 (parse input) @?= 12599655151
      -- solve2 (parse input) @?= 1
