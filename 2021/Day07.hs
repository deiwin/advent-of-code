module Day07 (main) where

import Control.Applicative (empty, (<|>))
import Control.Arrow (second, (>>>))
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

parse :: String -> [Int]
parse input = run $ number `P.sepBy1` P.char ',' <* eol <* P.eof
  where
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: [Int] -> _
solve1 input = minimum (flip cost input <$> range)
  -- input
  --   & fmap (abs . (midPoint -))
  --   & sum
  where
    cost midPoint = sum . fmap (abs . (midPoint -))
    range = [(minimum input)..(maximum input)]
    -- midPoint :: _
    -- midPoint = (fromIntegral (sum input) / fromIntegral (length input))
    -- midPoint = round $ fst $ L.foldl' f (head doubleInput, 1) (tail doubleInput)
    -- f :: (Double, Double) -> Double -> (Double, Double)
    -- f (avg, count) x = (((avg + (x / count)) / 2) , count + 1)
    -- doubleInput :: [Double]
    -- doubleInput = fromIntegral <$> input

main = do
  input <- readFile "inputs/Day07.txt"
  -- exampleInput <- readFile "inputs/Day07_example.txt"
  print $ solve1 $ parse input
  print $ solve1 [16,1,2,0,4,2,7,1,2,14]
  print $ sum [16,1,2,0,4,2,7,1,2,14]
  print $ (sum [16,1,2,0,4,2,7,1,2,14] `div` length [16,1,2,0,4,2,7,1,2,14])
  runTestTT $
    TestCase $ do
      1 @?= 2
      1 @?= 1
