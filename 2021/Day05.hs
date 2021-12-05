module Day05 (main) where

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
    rangeSize,
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
import Control.Monad (ap)
import Data.Tuple (swap)

parse :: String -> _
parse input = run $ do
  line `P.endBy1` eol <* P.eof
  where
    line = do
      from <- coord <* P.string " -> "
      to <- coord
      return (from, to)
    coord = do
      x <- number <* P.char ','
      y <- number
      return (V2 x y)
    -- Standard parsers
    letter = P.satisfy C.isLetter
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    spaces = P.many1 (P.char ' ')
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    longestMatch :: [(a, [b])] -> (a, [b])
    longestMatch = L.minimumBy (comparing (length . snd))
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: _
solve1 input =
  input
    & filter isHOrV
    & fmap sort
    & concatMap range
    & flip zip (repeat 1)
    & M.fromListWith (+)
    & M.filter (> 1)
    & M.size
  where
    sort (from, to)
      | from < to = (from, to)
      | otherwise = (to, from)
    isHOrV x = isHorizontal x || isVertical x
    isHorizontal (V2 x1 _, V2 x2 _) = x1 == x2
    isVertical (V2 _ y1, V2 _ y2) = y1 == y2

main = do
  input <- readFile "inputs/Day05.txt"
  -- exampleInput <- readFile "inputs/Day05_example.txt"
  print $ solve1 $ parse input
  runTestTT $
    TestCase $ do
      range (V2 950 698,V2 597 698) @?= []
      (range (V2 597 698,V2 950 698) /= [])  @?= True
      (V2 597 698 > V2 950 698)  @?= True

      1 @?= 1
