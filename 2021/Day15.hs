module Day15 (main) where

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
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MQ

type Grid = Array (V2 Int) Int

parse :: String -> [[Int]]
parse = fmap (fmap (read . (: []))) . lines

solve1 :: _
solve1 input = aStar grid (V2 0 0) (snd (A.bounds grid))
  where
    grid = buildGrid input

solve2 :: _
solve2 input = aStar grid (V2 0 0) (snd (A.bounds grid))
-- solve2 input = showGrid grid
  where
    grid = buildGrid newInput
    newInput :: [[Int]]
    newInput =
      input
        & fmap updateRow
        & replicate 5
        & flip zip [0..]
        & concatMap (\(xss, d) -> fmap (addToEachWithWraparound d) xss)
    updateRow :: [Int] -> [Int]
    updateRow row =
      row
        & replicate 5
        & zip [0..]
        & concatMap (uncurry addToEachWithWraparound)
    addToEachWithWraparound d xs = fmap (`add` d) xs
    add x d
      | x + d > 9 = x + d - 9
      | otherwise = x + d


aStar :: Grid -> V2 Int -> V2 Int -> _
aStar grid from to = go from S.empty 0 MQ.empty
  where
    go :: V2 Int -> Set (V2 Int) -> Int -> MinQueue _ -> Int
    go c visited cost toVisit
      | c == to = cost
      | otherwise =
        case MQ.minView newToConsiderToVisit of
          Nothing -> undefined
          Just ((_, _, newCost, newC), newToVisit) -> go newC newVisited newCost newToVisit
          -- Just ((_, _, newCost, newC), newToVisit) ->
          --   traceShow (newCost, newC) $ go newC newVisited newCost newToVisit
      where
        newToConsiderToVisit =
          surrounding grid c
            & filter (not . (`S.member` newVisited))
            & fmap toVisitFor
            & MQ.fromList
            & MQ.union toVisit
        toVisitFor c = (estCost, to - c, realCost, c)
          where
            realCost = cost + (grid A.! c)
            estCost = realCost + estimatedExtraCost c
        newVisited = S.insert c visited
    estimatedExtraCost :: V2 Int -> Int
    estimatedExtraCost c = case to - c of V2 y x -> y + x

surrounding :: Grid -> V2 Int -> [V2 Int]
surrounding grid coord =
  [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]
    & fmap (+ coord)
    & filter (inRange (A.bounds grid))

buildGrid :: [[Int]] -> Grid
buildGrid input = A.listArray bounds (concat input)
  where
    bounds = (V2 0 0, V2 (length input - 1) (length (head input) - 1))

showGrid :: Grid -> String
showGrid grid = unlines rows
  where
    bounds            = A.bounds grid
    (_, V2 maxY maxX) = bounds
    rows              = showRow <$> [0 .. maxY]
    showRow y = concatMap (showCell y) [0 .. maxX]
    showCell y x = show $ grid A.! V2 y x

main = do
  input <- readFile "inputs/Day15.txt"
  exampleInput <- readFile "inputs/Day15_example.txt"
  print $ solve2 $ parse input
  -- putStr $ solve2 $ parse exampleInput
  runTestTT $
    TestCase $ do
      1 @?= 40
      solve1 (parse exampleInput) @?= 40
      solve1 (parse input) @?= 702
      solve2 (parse input) @?= 2955
