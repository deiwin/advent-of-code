module Day08 (main) where

import Data.Array.IArray qualified as A
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Ix (inRange, range)
import Data.List (transpose)
import Data.List qualified as L
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [[Int]]
parse = fmap (fmap (read . L.singleton)) . lines

solve1 :: [[Int]] -> Int
solve1 input =
  input
    & rotationsWith lrtbSeen
    & L.foldl1' (mergeWith (||))
    & fmap (length . filter id)
    & sum
  where
    mergeWith :: (a -> a -> a) -> [[a]] -> [[a]] -> [[a]]
    mergeWith = L.zipWith . L.zipWith
    lrtbSeen :: [[Int]] -> [[Bool]]
    lrtbSeen = fmap (snd . L.mapAccumL f (-1))
      where
        f acc x = (max acc x, x > acc)

rotationsWith :: ([[a]] -> [[b]]) -> [[a]] -> [[[b]]]
rotationsWith f xss =
  rotationFs
    & fmap (\(to, from) -> from (f (to xss)))
  where
    rotationFs :: [([[a]] -> [[a]], [[b]] -> [[b]])]
    rotationFs =
      [ (id, id),
        (fmap reverse, fmap reverse),
        (transpose, transpose),
        (fmap reverse . transpose, transpose . fmap reverse)
      ]

type Coord = V2 Int

type Direction = V2 Int

type Grid = A.Array Coord Int

createGrid :: [[Int]] -> Grid
createGrid rows = A.listArray bounds $ concat rows
  where
    bounds = (V2 0 0, V2 (length rows - 1) (length (head rows) - 1))

allDirections :: [Direction]
allDirections =
  [ V2 (-1) 0,
    V2 0 (-1),
    V2 0 1,
    V2 1 0
  ]

scenicScore :: Grid -> Coord -> Int
scenicScore grid i =
  allDirections
    <&> dirScore
    & product
  where
    dirScore d =
      iterate (+ d) i
        & drop 1
        & span lower
        & score
    score (xs, y : _ys) = length xs + if inRange (A.bounds grid) y then 1 else 0
    score _ = undefined
    height = grid A.! i
    lower j =
      inRange (A.bounds grid) j
        && grid A.! j < height

solve2 :: [[Int]] -> Int
solve2 input =
  range (A.bounds grid)
    <&> scenicScore grid
    & maximum
  where
    grid = createGrid input

main = do
  input <- readFile "inputs/Day08.txt"
  exampleInput <- readFile "inputs/Day08_example.txt"
  runTestTT $
    TestCase $ do
      head (parse exampleInput) @?= [3, 0, 3, 7, 3]
      head (fmap reverse (parse exampleInput)) @?= [3, 7, 3, 0, 3]
      head (transpose (parse exampleInput)) @?= [3, 2, 6, 3, 3]
      head ((fmap reverse . transpose) (parse exampleInput)) @?= [3, 3, 6, 2, 3]
      all (== parse exampleInput) (rotationsWith id (parse exampleInput)) @?= True
      solve1 (parse exampleInput) @?= 21
      solve1 (parse input) @?= 1713
      solve2 (parse exampleInput) @?= 8
      solve2 (parse input) @?= 268464
