module Day07 (main) where

import Control.Arrow (second)
import Data.Foldable (find)
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Ix (inRange, range)
import Data.List qualified as L
import Data.Maybe (fromJust)
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

type Coord = V2 Int

data Cell = Start | Open | Splitter
  deriving (Show, Eq)

type Grid = HashMap Coord Cell

parse :: String -> ((Coord, Coord), Coord, Grid)
parse input = (bounds, start, grid)
  where
    grid =
      zip (range bounds) (concat rows)
        & HM.fromList
        <&> toCell
    start =
      grid
        & HM.toList
        & find ((== Start) . snd)
        & fromJust
        & fst
    rows = lines input
    bounds :: (Coord, Coord)
    bounds = (V2 0 0, V2 maxY maxX)
    maxY = length rows - 1
    maxX = length (head rows) - 1
    toCell = \case
      '.' -> Open
      'S' -> Start
      '^' -> Splitter
      _ -> undefined

step :: (Coord, Coord) -> Grid -> [Coord] -> (Int, [Coord])
step bounds grid rays = (splitCount, newRays)
  where
    splitCount = length splitterCoords
    movedRays =
      rays
        <&> (+ V2 1 0)
        & filter (inRange bounds)
        <&> (\c -> (c, grid HM.! c))
    splitterCoords =
      movedRays
        & filter ((== Splitter) . snd)
        <&> fst
    newRays =
      movedRays
        & concatMap maybeSplit
        & L.nub
        & filter (inRange bounds)
    maybeSplit = \case
      (c, Open) -> [c]
      (c, Splitter) -> (c +) <$> [V2 0 (-1), V2 0 1]
      _ -> undefined

solve1 :: ((Coord, Coord), Coord, Grid) -> Int
solve1 input =
  iterate (step bounds grid . snd) (0, [start])
    & takeWhile (not . null . snd)
    <&> fst
    & sum
  where
    (bounds, start, grid) = input

step' :: (Coord, Coord) -> Grid -> [(Int, Coord)] -> [(Int, Coord)]
step' bounds grid rays =
  rays
    <&> second (+ V2 1 0)
    & filter (inRange bounds . snd)
    & concatMap maybeSplit
    & filter (inRange bounds . snd)
    & L.groupBy ((==) `on` snd)
    <&> combine
  where
    maybeSplit (n, c) =
      case grid HM.! c of
        Open -> [(n, c)]
        Splitter -> [(n, c + V2 0 (-1)), (n, c + V2 0 1)]
        _ -> undefined
    combine :: [(Int, Coord)] -> (Int, Coord)
    combine xs = (sum (fst <$> xs), snd (head xs))

solve2 :: ((Coord, Coord), Coord, Grid) -> Int
solve2 input =
  iterate (step' bounds grid) [(1, start)]
    & takeWhile (not . null)
    & last
    <&> fst
    & sum
  where
    (bounds, start, grid) = input

main = do
  input <- readFile "inputs/Day07.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 1660
      solve2 (parse input) @?= 305999729392659
