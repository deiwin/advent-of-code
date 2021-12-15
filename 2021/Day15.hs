module Day15 (main) where

import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import Data.Function ((&))
import Data.Ix (inRange)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MQ
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

type Grid = Array (V2 Int) Int

parse :: String -> [[Int]]
parse = fmap (fmap (read . (: []))) . lines

solve1 :: [[Int]] -> Int
solve1 input = aStar grid (V2 0 0) (snd (A.bounds grid))
  where
    grid = buildGrid input

solve2 :: [[Int]] -> Int
solve2 input = aStar grid (V2 0 0) (snd (A.bounds grid))
  where
    grid = buildGrid newInput
    newInput :: [[Int]]
    newInput = growInput input

growInput :: [[Int]] -> [[Int]]
growInput input =
  input
    & fmap updateRow
    & replicate 5
    & flip zip [0 ..]
    & concatMap (\(xss, d) -> fmap (addToEachWithWraparound d) xss)
  where
    updateRow :: [Int] -> [Int]
    updateRow row =
      row
        & replicate 5
        & zip [0 ..]
        & concatMap (uncurry addToEachWithWraparound)
    addToEachWithWraparound d xs = fmap (`add` d) xs
    add x d
      | x + d > 9 = x + d - 9
      | otherwise = x + d

aStar :: Grid -> V2 Int -> V2 Int -> Int
aStar grid from to = go from S.empty 0 MQ.empty
  where
    go :: V2 Int -> Set (V2 Int) -> Int -> MinQueue (Int, V2 Int, Int, V2 Int) -> Int
    go c visited cost toVisit
      | c == to = cost
      | otherwise =
        case MQ.minView newToConsiderToVisit of
          Nothing -> undefined
          Just ((_, _, newCost, newC), newToVisit) -> go newC newVisited newCost newToVisit
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
    bounds = A.bounds grid
    (_, V2 maxY maxX) = bounds
    rows = showRow <$> [0 .. maxY]
    showRow y = concatMap (showCell y) [0 .. maxX]
    showCell y x = show $ grid A.! V2 y x

main = do
  input <- readFile "inputs/Day15.txt"
  exampleInput <- readFile "inputs/Day15_example.txt"
  putStr $ showGrid $ buildGrid $ growInput $ parse exampleInput
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 40
      solve1 (parse input) @?= 702
      solve2 (parse exampleInput) @?= 315
      solve2 (parse input) @?= 2955
