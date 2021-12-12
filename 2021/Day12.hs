module Day12 (main) where

import Control.Arrow (second)
import qualified Data.Char as C
import Data.Function ((&))
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple (swap)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P

type Edge = (String, String)

type Graph = Map String (Set String)

type Visited = Map String Int

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

solve1 :: [Edge] -> Int
solve1 input = length $ dffWith visitable graph
  where
    visitable visited toVisit
      | all C.isUpper toVisit = True
      | toVisit `M.member` visited = False
      | otherwise = True
    graph = mkGraph input

solve2 :: [Edge] -> Int
solve2 input = length $ dffWith visitable graph
  where
    visitable visited toVisit = isBigCave || canVisitSmallCave
      where
        isBigCave = all C.isUpper toVisit
        canVisitSmallCave = notVisitedYet || canVisitTwice
        notVisitedYet = fromMaybe 0 (M.lookup toVisit visited) < 1
        canVisitTwice =
          toVisit `notElem` ["start", "end"]
            && noSmallCaveHasBeenVisitedTwiceYet
        noSmallCaveHasBeenVisitedTwiceYet =
          visited
            & M.filterWithKey (\k _ -> all C.isLower k)
            & M.filter (> 1)
            & M.null
    graph = mkGraph input

dffWith :: (Visited -> String -> Bool) -> Graph -> [Visited]
dffWith visitable = go "start" M.empty
  where
    go from@"end" visited _ = [M.insertWith (+) from 1 visited]
    go from visited graph = concatMap (\to -> go to newVisited graph) toVisit
      where
        newVisited = M.insertWith (+) from 1 visited
        toVisit =
          from
            & (graph M.!)
            & S.toList
            & filter (visitable newVisited)

mkGraph :: [Edge] -> Graph
mkGraph input =
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
      solve2 (parse input) @?= 117095
