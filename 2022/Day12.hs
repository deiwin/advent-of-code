module Day12 (main) where

import Data.Array.IArray qualified as A
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Ix (inRange)
import Data.List qualified as L
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.PQueue.Min (MinQueue)
import Data.PQueue.Min qualified as MQ
import Data.Set (Set)
import Data.Set qualified as S
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [String]
parse = lines

type Coord = V2 Int

type Grid = A.Array Coord Int

data State = State
  { current :: Coord,
    target :: Coord,
    grid :: Grid
  }
  deriving (Eq, Show)

toState :: [String] -> State
toState input = State {..}
  where
    grid =
      initialGrid
        & (A.// [(current, 'a'), (target, 'z')])
        & A.amap (flip (-) (C.ord 'a') . C.ord)
    target =
      initialGrid
        & A.assocs
        & L.find ((== 'E') . snd)
        & fromJust
        & fst
    current =
      initialGrid
        & A.assocs
        & L.find ((== 'S') . snd)
        & fromJust
        & fst
    initialGrid :: A.Array Coord Char
    initialGrid =
      input
        & concat
        & A.listArray bounds
    bounds = (V2 0 0, V2 (length input - 1) (length (head input) - 1))

aStar :: State -> Int
aStar (State {grid, current, target}) = go current S.empty S.empty 0 MQ.empty
  where
    go :: Coord -> Set Coord -> Set (Int, Coord) -> Int -> MinQueue (Int, Coord, Int, Coord) -> Int
    go c visited consideredOptions cost toVisit
      | c == target = cost
      | otherwise =
          case MQ.minView newToConsiderToVisit of
            Nothing -> error "No nodes to visit"
            Just ((_, _, newCost, newC), newToVisit) -> go newC newVisited newConsideredOptions newCost newToVisit
      where
        newVisited = S.insert c visited
        newOptions =
          surrounding grid c
            & filter (not . (`S.member` newVisited))
            & filter (not . (`S.member` consideredOptions) . (cost + 1,))
            & filter (\option -> grid A.! option - grid A.! c <= 1)
        newToConsiderToVisit =
          newOptions
            & fmap toVisitFor
            & MQ.fromList
            & MQ.union toVisit
        newConsideredOptions =
          newOptions
            <&> (cost + 1,)
            & S.fromList
            & S.union consideredOptions
        toVisitFor c = (estCost, target - c, cost + 1, c)
          where
            realCost = cost + 1
            estCost = realCost + estimatedExtraCost c
    estimatedExtraCost :: Coord -> Int
    estimatedExtraCost c = manhattan $ target - c
    manhattan (V2 y x) = y + x

surrounding :: Grid -> Coord -> [Coord]
surrounding grid coord =
  [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]
    & fmap (+ coord)
    & filter (inRange (A.bounds grid))

solve1 :: [String] -> Int
solve1 input =
  input
    & toState
    & aStar

aStar' :: State -> Int
aStar' (State {grid, target = current}) = go current S.empty S.empty 0 MQ.empty
  where
    go :: Coord -> Set Coord -> Set (Int, Coord) -> Int -> MinQueue (Int, Int, Coord, Int, Coord) -> Int
    go !c !visited !consideredOptions !cost !toVisit
      | c `elem` allTargets = cost
      | otherwise =
          case MQ.minView newToConsiderToVisit of
            Nothing -> error "No nodes to visit"
            Just ((_, _, _, newCost, newC), newToVisit) -> go newC newVisited newConsideredOptions newCost newToVisit
      where
        newVisited = S.insert c visited
        newOptions =
          surrounding grid c
            & filter (not . (`S.member` newVisited))
            & filter (not . (`S.member` consideredOptions) . (cost + 1,))
            & filter (\option -> grid A.! option - grid A.! c >= (-1))
        newToConsiderToVisit =
          newOptions
            & fmap toVisitFor
            & MQ.fromList
            & MQ.union toVisit
        newConsideredOptions =
          newOptions
            <&> (cost + 1,)
            & S.fromList
            & S.union consideredOptions
        toVisitFor c = (grid A.! c, estCost, closestTarget c - c, cost + 1, c)
          where
            realCost = cost + 1
            estCost = realCost + estimatedExtraCost c
    closestTarget :: Coord -> Coord
    closestTarget c = L.minimumBy (comparing (manhattan . flip (-) c)) allTargets
    estimatedExtraCost :: Coord -> Int
    estimatedExtraCost = manhattan . closestTarget
    manhattan (V2 y x) = y + x
    allTargets :: [Coord]
    allTargets =
      grid
        & A.assocs
        & filter ((== 0) . snd)
        <&> fst

solve2 :: [String] -> Int
solve2 input =
  input
    & toState
    & aStar'

main = do
  input <- readFile "inputs/Day12.txt"
  exampleInput <- readFile "inputs/Day12_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 31
      solve1 (parse input) @?= 339
      solve2 (parse exampleInput) @?= 29
      solve2 (parse input) @?= 332
