module Day23 (main) where

import Control.Arrow (first, (>>>))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HM
import Data.Ix (rangeSize)
import Data.List qualified as L
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> Set (V2 Int)
parse =
  lines
    >>> zip [0 ..]
    >>> concatMap (\(y, cs) -> first (V2 y) <$> zip [0 ..] cs)
    >>> filter ((== '#') . snd)
    >>> fmap fst
    >>> S.fromList

data State = State
  { elves :: Set (V2 Int),
    directions :: [Set (V2 Int) -> V2 Int -> Maybe (V2 Int)]
  }

initialState :: Set (V2 Int) -> State
initialState elves = State {..}
  where
    directions =
      [ (V2 (-1) 0, [V2 (-1) x | x <- [-1, 0, 1]]),
        (V2 1 0, [V2 1 x | x <- [-1, 0, 1]]),
        (V2 0 (-1), [V2 y (-1) | y <- [-1, 0, 1]]),
        (V2 0 1, [V2 y 1 | y <- [-1, 0, 1]])
      ]
        <&> f
    f (diff, checkDiffs) elves c
      | any ((`S.member` elves) . (+ c)) checkDiffs = Nothing
      | otherwise = Just (diff + c)

allDirections :: [V2 Int]
allDirections = [V2 y x | y <- [-1, 0, 1], x <- [-1, 0, 1], (y, x) /= (0, 0)]

rotate :: [a] -> [a]
rotate xs = tail xs ++ [head xs]

move :: State -> State
move s = State {..}
  where
    elves =
      s.elves
        & S.toList
        <&> (\c -> (proposal c, [c]))
        & HM.fromListWith (++)
        & HM.foldlWithKey' f S.empty
    directions = rotate s.directions
    proposal :: V2 Int -> V2 Int
    proposal c
      | all (empty . (+ c)) allDirections = c
      | otherwise = head ((++ [c]) $ mapMaybe (\f -> f s.elves c) s.directions)
    empty c = S.notMember c s.elves
    f :: Set (V2 Int) -> V2 Int -> [V2 Int] -> Set (V2 Int)
    f s to = \case
      [_] -> S.insert to s
      froms -> S.union (S.fromList froms) s

score :: State -> Int
score s =
  s.elves
    & S.foldl' f (S.findMin s.elves, S.findMin s.elves)
    & rangeSize
    & (+ ((-1) * S.size s.elves))
  where
    f :: (V2 Int, V2 Int) -> V2 Int -> (V2 Int, V2 Int)
    f (V2 minY minX, V2 maxY maxX) (V2 y x) =
      ( V2 (min minY y) (min minX x),
        V2 (max maxY y) (max maxX x)
      )

solve1 :: Set (V2 Int) -> Int
solve1 input =
  input
    & initialState
    & iterate move
    & (L.!! 10)
    & score

solve2 :: Set (V2 Int) -> Int
solve2 input =
  input
    & initialState
    & iterate move
    & stopCount
  where
    stopCount :: [State] -> Int
    stopCount xs =
      zip3 [1 ..] xs (drop 1 xs)
        & L.find (\(_, s, nextS) -> s.elves == nextS.elves)
        & fromJust
        & (\(i, _, _) -> i)

main = do
  input <- readFile "inputs/Day23.txt"
  exampleInput <- readFile "inputs/Day23_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 110
      solve1 (parse input) @?= 4116
      solve2 (parse exampleInput) @?= 20
      solve2 (parse input) @?= 984
