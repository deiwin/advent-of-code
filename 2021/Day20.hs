module Day20 (main) where

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
import Data.Bits (Bits)
import qualified Data.Bits as Bits
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
import Data.Bifunctor (bimap)

parse :: String -> ([Bool], [[Bool]])
parse input = run $ do
  enhancementAlgo <- P.many1 pixel <* P.count 2 eol
  inputImage <- P.many1 pixel `P.endBy1` eol <* P.eof
  return (enhancementAlgo, inputImage)
  where
    pixel =
      False <$ P.char '.'
        <|> True <$ P.char '#'
    -- Standard parsers
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: _
solve1 input = S.size (runN input 2)

runN :: ([Bool], [[Bool]]) -> Int -> Set (V2 Int)
runN input n =
  (False, grid)
    & iterate (enhance algo)
    & (!! n)
    & snd
  where
    algo = buildAlgo (fst input)
    grid = buildGrid (snd input)

enhance :: Set Int -> (Bool, Set (V2 Int)) -> (Bool, Set (V2 Int))
enhance algo (infinityLit, grid) = (newInfinityLit, newGrid)
  where
    newInfinityLit
      | infinityLit = 511 `S.member` algo
      | otherwise = 0 `S.member` algo
    newGrid =
      grid
        & gridBounds
        & bimap (flip (-) (V2 1 1)) (+ V2 1 1)
        & range
        & fmap (toSecond (shouldLight . toInt . toBinary . surrounding))
        & filter snd
        & fmap fst
        & S.fromList
    shouldLight = (`S.member` algo)
    toBinary = fmap toBool
    toBool c
      | inRange bounds c = c `S.member` grid
      | otherwise = infinityLit
    toSecond f a = (a, f a)
    bounds = gridBounds grid
    surrounding' a = range (a - V2 10 10, a + V2 10 10)

toInt :: [Bool] -> Int
toInt =
  reverse
    >>> zip [0 ..]
    >>> filter snd
    >>> fmap fst
    >>> foldr (flip Bits.setBit) 0

surrounding :: V2 Int -> [V2 Int]
surrounding a = range (a - V2 1 1, a + V2 1 1)

buildAlgo :: [Bool] -> Set Int
buildAlgo = S.fromList . fmap fst . filter snd . zip [0 ..]

buildGrid :: [[Bool]] -> Set (V2 Int)
buildGrid pixels = S.fromList $ do
  (y, row) <- zip [0 ..] pixels
  (x, pixel) <- zip [0 ..] row
  guard pixel
  return (V2 y x)

showGrid :: Set (V2 Int) -> String
showGrid grid =
  grid
    & gridBounds
    & range
    & L.groupBy equalYs
    & fmap (fmap showDot)
    & unlines
  where
    showDot c
      | c `S.member` grid = '#'
      | otherwise = '.'
    equalYs (V2 y1 _) (V2 y2 _) = y1 == y2

gridBounds :: Set (V2 Int) -> (V2 Int, V2 Int)
gridBounds grid = (minBound, maxBound)
  where
    minBound = V2 (minimum (getY <$> S.toList grid)) (minimum (getX <$> S.toList grid))
    maxBound = V2 (maximum (getY <$> S.toList grid)) (maximum (getX <$> S.toList grid))
    getY (V2 y _) = y
    getX (V2 _ x) = x

main = do
  input <- readFile "inputs/Day20.txt"
  exampleInput <- readFile "inputs/Day20_example.txt"
  -- print $ solve1 $ parse exampleInput
  putStrLn (showGrid (runN (parse input) 2))
  -- putStrLn (showGrid (runN (parse exampleInput) 2))
  -- putStrLn (showGrid (runN (parse exampleInput) 20))
  runTestTT $
    TestCase $ do
      surrounding (V2 0 0) @?= [V2 (-1) (-1), V2 (-1) 0, V2 (-1) 1, V2 0 (-1), V2 0 0, V2 0 1, V2 1 (-1), V2 1 0, V2 1 1]
      solve1 (parse exampleInput) @?= 35
      solve1 (parse input) @?= 5488
