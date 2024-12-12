module Day12 (main) where

import Data.Array.IArray qualified as A
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Ix (inRange, range)
import Data.List (foldl')
import Data.Set (Set)
import Data.Set qualified as S
import Linear.V2 (V2 (..), perp)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [[Char]]
parse = lines

type Coord = V2 Int

type Grid = A.Array Coord Char

readInput :: [[Char]] -> Grid
readInput rows = A.listArray bounds $ concat rows
  where
    bounds = (V2 0 0, V2 (length rows - 1) (length (head rows) - 1))

directions :: [V2 Int]
directions =
  [ V2 (-1) 0, -- up
    V2 1 0, -- down
    V2 0 (-1), -- left
    V2 0 1 -- right
  ]

type Plot = [Coord]

area :: Plot -> Int
area = length

perimeter :: Grid -> Plot -> Int
perimeter grid plot =
  neighbors
    & filter notInPlot
    & length
  where
    neighbors = concatMap (\c -> directions <&> (c +)) plot
    notInPlot c = not (inRange (A.bounds grid) c) || ((grid A.! c) /= plant)
    plant = grid A.! head plot

cost :: Grid -> Plot -> Int
cost grid plot = area plot * perimeter grid plot

findPlot :: Grid -> Coord -> Plot
findPlot grid c = S.toList $ go grid [c] S.empty
  where
    go :: Grid -> [Coord] -> Set Coord -> Set Coord
    go _grid [] visited = visited
    go grid (c : cs) visited
      | not (inRange (A.bounds grid) c) = go grid cs visited
      | c `S.member` visited = go grid cs visited
      | grid A.! c /= plant = go grid cs visited
      | otherwise = go grid ((directions <&> (c +)) ++ cs) (S.insert c visited)
    plant = grid A.! c

solve1 :: [[Char]] -> Int
solve1 input =
  range bounds
    & foldl' f ([], S.empty)
    & fst
    <&> cost grid
    & sum
  where
    grid = readInput input
    bounds = A.bounds grid
    f (plots, visited) c
      | c `S.member` visited = (plots, visited)
      | otherwise = (plot : plots, visited <> S.fromList plot)
      where
        plot = findPlot grid c

cornerDiffs :: [[V2 Int]]
cornerDiffs =
  [ V2 (-1) 0, -- up
    V2 (-1) 1, -- up right
    V2 0 1 -- right
  ]
    & iterate (fmap perp)
    & take 4

sideCount :: Plot -> Int
sideCount plot =
  plot
    & concatMap (filter isCorner . (\c -> fmap (c +) <$> cornerDiffs))
    & length
  where
    plotS = S.fromList plot
    inPlot c = c `S.member` plotS
    isCorner cs = inPlotCount == 0 || middleOut || middleIn
      where
        inPlotCount = length $ filter inPlot cs
        [a, b, c] = cs
        middleOut = inPlot a && not (inPlot b) && inPlot c
        middleIn = not (inPlot a) && inPlot b && not (inPlot c)

cost' :: Plot -> Int
cost' plot = area plot * sideCount plot

solve2 :: [[Char]] -> Int
solve2 input =
  range bounds
    & foldl' f ([], S.empty)
    & fst
    <&> cost'
    & sum
  where
    grid = readInput input
    bounds = A.bounds grid
    f (plots, visited) c
      | c `S.member` visited = (plots, visited)
      | otherwise = (plot : plots, visited <> S.fromList plot)
      where
        plot = findPlot grid c

main = do
  input <- readFile "inputs/Day12.txt"
  exampleInput <- readFile "inputs/Day12_example.txt"
  exampleInput2 <- readFile "inputs/Day12_example2.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 140
      solve1 (parse exampleInput2) @?= 1930
      solve1 (parse input) @?= 1473408
      solve2 (parse exampleInput) @?= 80
      solve2 (parse exampleInput2) @?= 1206
      solve2 (parse input) @?= 886364
