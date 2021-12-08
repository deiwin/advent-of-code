module Day08 (main) where

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
import Data.Either (fromLeft, isLeft, isRight, partitionEithers)
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

type Entry = ([String], [String])

type DisplayState = Either (String, Int) (String, [Int])

parse :: String -> [Entry]
parse input = run $ entry `P.endBy1` eol <* P.eof
  where
    entry = do
      signal <- pattern `P.sepBy1` spaces <* P.string " | "
      output <- pattern `P.sepBy1` spaces
      return (signal, output)
    pattern = P.many1 letter
    -- Standard parsers
    letter = P.satisfy C.isLetter
    spaces = P.many1 (P.char ' ')
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: [Entry] -> Int
solve1 input =
  input
    & concatMap snd
    & filter ((`elem` uniqueCardinalities) . length)
    & length
  where
    uniqueCardinalities =
      numberCardinalities
        & filter ((`elem` [1, 4, 7, 8]) . fst)
        & fmap snd

solve2 :: [Entry] -> _
solve2 input =
  input
    & fmap (toInt . deduceNumbers)
    & sum

toInt :: [Int] -> Int
toInt =
  reverse
    >>> zip [0..]
    >>> fmap (uncurry (*) . first (10 ^))
    >>> sum

deduceNumbers :: Entry -> _ -- [Int]
deduceNumbers (signal, output) = ((map M.!) . S.fromList) <$> output
  where
    map =
      M.fromList
        [ (S.fromList c0, 0),
          (S.fromList c1, 1),
          (S.fromList c2, 2),
          (S.fromList c3, 3),
          (S.fromList c4, 4),
          (S.fromList c5, 5),
          (S.fromList c6, 6),
          (S.fromList c7, 7),
          (S.fromList c8, 8),
          (S.fromList c9, 9)
        ]
    c1 = uniqueCardinalityNr 1
    c4 = uniqueCardinalityNr 4
    c7 = uniqueCardinalityNr 7
    c8 = uniqueCardinalityNr 8
    c235 = cardinalityNr 2
    [c3] = filter (includes c1) c235
    c25 = c235 L.\\ [c3]
    [c5] = filter (L.null . (L.\\ c6)) c25
    [c2] = c25 L.\\ [c5]
    c069 = cardinalityNr 0
    [c6] = filter (not . includes c1) c069
    [c9] = filter (includes c4) c069
    [c0] = c069 L.\\ [c6, c9]
    uniqueCardinalityNr x =
      signal
        & L.find ((== length (canonicalNames M.! x)) . length)
        & fromJust
    cardinalityNr x = filter ((== length (canonicalNames M.! x)) . length) signal
    includes xs inYs = (xs `L.intersect` inYs) == xs

-- deduceNumbers :: Entry -> _ -- [Int]
-- deduceNumbers (signal, output) =
--   signal
--     & fmap addInitialPossibilities
--     & goRound
--   where
--     addInitialPossibilities segment =
--       numberCardinalities
--         & filter ((== length segment) . snd)
--         & fmap fst
--         & (segment,)
--         & (\(seg, xs@(x:_)) -> if length xs == 1 then Left (seg, x) else Right (seg, xs))
--     goRound :: [DisplayState] -> _
--     goRound segmentList
--       -- | all isLeft segmentList = segmentList
--       | otherwise -- TODO recurse
--         =
--           knownSegments
--             -- & fmap (second (canonicalNames M.!))
--       where
--         (knownSegments, undeterminedSegments) = partitionEithers segmentList
    -- reducePossibilities knownSegments (segment, possibilities) = (segment, possibilities, knownSegments)

-- countCardinality x = (x, length x)

canonicalNames :: Map Int String
canonicalNames =
  M.fromList
    [ (0, "abcefg"),
      (1, "cf"),
      (2, "acdeg"),
      (3, "acdfg"),
      (4, "bcdf"),
      (5, "abdfg"),
      (6, "abdefg"),
      (7, "acf"),
      (8, "abcdefg"),
      (9, "abcdfg")
    ]
numberCardinalities = second length <$> M.toList canonicalNames

main = do
  input <- readFile "inputs/Day08.txt"
  exampleInput <- readFile "inputs/Day08_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 349
      solve2 (parse exampleInput) @?= 61229
      solve2 (parse input) @?= 61229
