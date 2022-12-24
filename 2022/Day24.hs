module Day24 (main) where

import Control.Arrow (first, second)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Ix (inRange)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.PQueue.Min (MinQueue)
import Data.PQueue.Min qualified as MQ
import Data.Set (Set)
import Data.Set qualified as S
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Prelude hiding (Either (..))

data Dir = Up | Right | Down | Left
  deriving (Eq, Show, Ord)

type Coord = V2 Int

data State = State
  { start :: Coord,
    target :: Coord,
    currentLocation :: Coord,
    bounds :: (Coord, Coord),
    blizzards :: HashMap Coord [Dir]
  }
  deriving (Eq, Show, Ord)

parse :: String -> State
parse input = State {..}
  where
    start =
      charMap
        & M.filter (== '.')
        & M.findMin
        & fst
    currentLocation = start
    target =
      charMap
        & M.filter (== '.')
        & M.findMax
        & fst
    bounds = (fst (M.findMin innerCharMap), fst (M.findMax innerCharMap))
    blizzards =
      innerCharMap
        & M.filter (/= '.')
        & M.toList
        <&> second (L.singleton . toCell)
        & HM.fromList
    innerCharMap =
      charMap
        & M.filterWithKey f
      where
        f (V2 y x) _ = y `notElem` [minY, maxY] && x `notElem` [minX, maxX]
        (V2 minY minX, _) = M.findMin charMap
        (V2 maxY maxX, _) = M.findMax charMap
    charMap =
      input
        & lines
        & zip [0 ..]
        & concatMap (\(y, cs) -> first (V2 y) <$> zip [0 ..] cs)
        & M.fromList
    toCell = \case
      '^' -> Up
      '>' -> Right
      'v' -> Down
      '<' -> Left
      _ -> undefined

moveOptions :: State -> [State]
moveOptions s =
  [V2 0 0, V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]
    <&> (+ s.currentLocation)
    & filter validPosition
    & filter (not . (`HM.member` afterBlizMove.blizzards))
    <&> (\c -> afterBlizMove {currentLocation = c})
  where
    validPosition c =
      c == s.start
        || c == s.target
        || inRange s.bounds c
    afterBlizMove = moveBlizzards s

moveBlizzards :: State -> State
moveBlizzards s =
  s.blizzards
    & HM.toList
    & concatMap (\(c, blizs) -> (c,) <$> blizs)
    <&> (second L.singleton . move)
    & HM.fromListWith (++)
    & (\blizs -> s {blizzards = blizs})
  where
    move :: (Coord, Dir) -> (Coord, Dir)
    move (c@(V2 y x), dir)
      | inRange s.bounds (naiveMove (c, dir)) = (naiveMove (c, dir), dir)
      | otherwise =
          case dir of
            Up -> (V2 maxY x, dir)
            Right -> (V2 y minX, dir)
            Down -> (V2 minY x, dir)
            Left -> (V2 y maxX, dir)
    naiveMove :: (Coord, Dir) -> Coord
    naiveMove = \case
      (c, Up) -> c + V2 (-1) 0
      (c, Right) -> c + V2 0 1
      (c, Down) -> c + V2 1 0
      (c, Left) -> c + V2 0 (-1)
    (V2 minY minX, V2 maxY maxX) = s.bounds

aStar :: State -> (Int, State)
aStar s = go s S.empty 0 MQ.empty
  where
    go :: State -> Set (Int, Coord) -> Int -> MinQueue (Int, Int, State) -> (Int, State)
    go s considered cost toVisit
      | s.currentLocation == s.target = (cost, s)
      | otherwise =
          case MQ.minView newToConsiderToVisit of
            Nothing -> error "No nodes to visit"
            Just ((_, newCost, newS), newToVisit) -> go newS newConsidered newCost newToVisit
      where
        newOptions =
          moveOptions s
            & filter (not . (`S.member` considered) . (cost + 1,) . currentLocation)
        newToConsiderToVisit =
          newOptions
            & fmap toVisitFor
            & MQ.fromList
            & MQ.union toVisit
        newConsidered =
          newOptions
            <&> (cost + 1,)
            <&> second currentLocation
            & S.fromList
            & S.union considered
        toVisitFor s = (estCost, cost + 1, s)
          where
            realCost = cost + 1
            estCost = realCost + estimatedExtraCost s.currentLocation
    estimatedExtraCost :: Coord -> Int
    estimatedExtraCost c = manhattan $ s.target - c
    manhattan (V2 y x) = y + x

solve1 :: State -> Int
solve1 input =
  input
    & aStar
    & fst

solve2 :: State -> Int
solve2 initialState =
  initialState
    & aStar
    & (\(x, s) -> first (+ x) $ aStar (s {start = initialState.target, target = initialState.start}))
    & (\(x, s) -> first (+ x) $ aStar (s {start = initialState.start, target = initialState.target}))
    & fst

main = do
  input <- readFile "inputs/Day24.txt"
  exampleInput <- readFile "inputs/Day24_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 18
      solve1 (parse input) @?= 343
      solve2 (parse exampleInput) @?= 54
      solve2 (parse input) @?= 960
