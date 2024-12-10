module Day10 (main) where

import Data.Array.IArray qualified as A
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Ix (inRange, range)
import Data.List (foldl')
import Data.Set qualified as S
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [[Int]]
parse input = fmap (read . (: [])) <$> lines input

type Coord = V2 Int

type Grid = A.Array Coord Int

readInput :: [[Int]] -> Grid
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

solve1 :: [[Int]] -> Int
solve1 input =
  trailheads
    <&> score
    & sum
  where
    grid = readInput input
    bounds = A.bounds grid
    trailheads =
      range bounds
        & filter (\coord -> grid A.! coord == 0)
    score c = S.size $ score' c 0
    score' c n
      | not (inRange bounds c) = S.empty
      | grid A.! c /= n = S.empty
      | n == 9 = S.singleton c
      | otherwise =
          directions
            <&> (\c' -> score' c' (n + 1)) . (c +)
            & foldl' S.union S.empty

solve2 :: [[Int]] -> Int
solve2 input =
  trailheads
    <&> score
    & sum
  where
    grid = readInput input
    bounds = A.bounds grid
    trailheads =
      range bounds
        & filter (\coord -> grid A.! coord == 0)
    score c = score' c 0
    score' c n
      | not (inRange bounds c) = 0
      | grid A.! c /= n = 0
      | n == 9 = 1
      | otherwise =
          directions
            <&> (\c' -> score' c' (n + 1)) . (c +)
            & sum

main = do
  input <- readFile "inputs/Day10.txt"
  exampleInput <- readFile "inputs/Day10_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 36
      solve1 (parse input) @?= 811
      solve2 (parse exampleInput) @?= 81
      solve2 (parse input) @?= 1794
