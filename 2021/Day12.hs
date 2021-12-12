module Day12 (main) where

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
import qualified Data.Graph as G
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
    fromMaybe,
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
import Data.Tuple (swap)
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

type Edge = (String, String)

type Graph = Map String (Set String)

type Path = [String]

parse :: String -> [Edge]
parse input = run (edge `P.endBy1` eol <* P.eof)
  where
    edge = do
      from <- P.many1 letter <* P.char '-'
      to <- P.many1 letter
      return (from, to)
    -- Standard parsers
    letter = P.satisfy C.isLetter
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: _
solve1 input = length $ dff graph
  where
    dff :: Graph -> [Path]
    dff = go "start" []
      where
        go from@"end" visited _ = [from : visited]
        go from visited g = concatMap (\to -> go to newVisited g) toVisit
          where
            newVisited = from : visited
            toVisit =
              from
                & (g M.!)
                & S.toList
                & filter visitable
            visitable to
              | all C.isUpper to = True
              | to `L.elem` visited = False
              | otherwise = True

    graph :: Graph
    graph =
      input
        & (++ (swap <$> input))
        & fmap (second S.singleton)
        & M.fromListWith S.union

solve2 :: _
solve2 input = length $ dff graph
  where
    dff :: Graph -> [Path]
    dff = go "start" []
      where
        go from@"end" visited _ = [from : visited]
        go from visited g = concatMap (\to -> go to newVisited g) toVisit
          where
            newVisited = from : visited
            toVisit =
              from
                & (g M.!)
                & S.toList
                & filter visitable
            visitable to =
              all C.isUpper to
                || canVisitSmallCave
              where
                canVisitSmallCave = notVisitedYet || canVisitTwice
                notVisitedYet = fromMaybe 0 (M.lookup to visitedCountMap) < 1
                canVisitTwice =
                  to `notElem` ["start", "end"]
                    && noSmallCaveHasBeenVisitedTwiceYet
                noSmallCaveHasBeenVisitedTwiceYet =
                  visitedCountMap
                    & M.filterWithKey (\k _ -> all C.isLower k)
                    & M.filter (> 1)
                    & M.null
                visitedCountMap =
                  newVisited
                    & flip zip (repeat 1)
                    & M.fromListWith (+)
    graph :: Graph
    graph =
      input
        & (++ (swap <$> input))
        & fmap (second S.singleton)
        & M.fromListWith S.union

main = do
  input <- readFile "inputs/Day12.txt"
  exampleInput1 <- readFile "inputs/Day12_example1.txt"
  exampleInput2 <- readFile "inputs/Day12_example2.txt"
  exampleInput3 <- readFile "inputs/Day12_example3.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput1) @?= 10
      solve1 (parse exampleInput2) @?= 19
      solve1 (parse exampleInput3) @?= 226
      solve1 (parse input) @?= 4885
      solve2 (parse exampleInput1) @?= 36
      solve2 (parse exampleInput2) @?= 103
      solve2 (parse exampleInput3) @?= 3509
      solve2 (parse input) @?= 4885
