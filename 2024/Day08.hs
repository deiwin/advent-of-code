module Day08 (main) where

import Data.Function ((&))
import Data.Ix (inRange, range)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [[Char]]
parse = lines

type Coord = V2 Int

readInput :: [[Char]] -> (Map Char [Coord], (Coord, Coord))
readInput rows = (antennas, bounds)
  where
    antennas =
      zip (concat rows) (range bounds)
        & mapMaybe
          ( \(c, coord) ->
              ( case c of
                  '.' -> Nothing
                  _ -> Just c
              )
                & fmap (,[coord])
          )
        & M.fromListWith (++)
    bounds = (V2 0 0, V2 (length rows - 1) (length (head rows) - 1))

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

antinodes :: [Coord] -> [Coord]
antinodes cs =
  cs
    & pairs
    & concatMap resonances
  where
    resonances (a, b) =
      let diff = b - a
       in [a - diff, b + diff]

solve1 :: [[Char]] -> Int
solve1 input =
  antennas
    & M.elems
    & concatMap (filter (inRange bounds) . antinodes)
    & S.fromList
    & S.size
  where
    (antennas, bounds) = readInput input

antinodes' :: (Coord, Coord) -> [Coord] -> [Coord]
antinodes' bounds cs =
  cs
    & pairs
    & concatMap resonances
  where
    resonances (a, b) =
      start
        & iterate (+ diff)
        & takeWhile (inRange bounds)
      where
        diff = b - a
        start =
          a
            & iterate (\c -> c - diff)
            & takeWhile (inRange bounds)
            & last

solve2 :: [[Char]] -> Int
solve2 input =
  antennas
    & M.elems
    & concatMap (antinodes' bounds)
    & S.fromList
    & S.size
  where
    (antennas, bounds) = readInput input

main = do
  input <- readFile "inputs/Day08.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 371
      solve2 (parse input) @?= 1229
