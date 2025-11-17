module Day20 (main) where

import Data.Array.IArray qualified as A
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Ix (inRange)
import Data.List (find, foldl', unfoldr)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [[Char]]
parse = lines

type Coord = V2 Int

type Grid = A.Array Coord Cell

data Cell = Wall | Start | End | Open
  deriving (Show, Eq)

readInput :: [[Char]] -> (Grid, Coord)
readInput input = (grid, start)
  where
    grid = A.listArray bounds $ toCell <$> concat input
    bounds = (V2 0 0, V2 (length input - 1) (length (head input) - 1))
    toCell = \case
      '#' -> Wall
      '.' -> Open
      'S' -> Start
      'E' -> End
      _ -> error "unknown cell"
    start = fst $ fromJust $ find ((== Start) . snd) $ A.assocs grid

track :: Grid -> Coord -> [Coord]
track grid start = unfoldr go (Just (start, start))
  where
    go :: Maybe (Coord, Coord) -> Maybe (Coord, Maybe (Coord, Coord))
    go Nothing = Nothing
    go (Just (current, previous))
      | grid A.! current == End = Just (current, Nothing)
      | otherwise =
          move1
            & fmap (+ current)
            & filter (/= previous)
            & find ((`elem` [Open, End]) . (grid A.!))
            & fromJust
            & (\c -> Just (current, Just (c, current)))

move1 :: [Coord]
move1 =
  [ V2 (-1) 0,
    V2 1 0,
    V2 0 1,
    V2 0 (-1)
  ]

cheats :: Int -> [Coord] -> [Int]
cheats maxCheatDist track = concat $ zipWith go track (drop 1 $ L.tails track)
  where
    go :: Coord -> [Coord] -> [Int]
    go from rest =
      zip rest [1 ..]
        & fmap (\(to, originalDist) -> (originalDist, manhattan (to - from)))
        & filter (\(_, mDist) -> mDist <= maxCheatDist)
        & fmap (uncurry (-))
        & filter (> 0)

manhattan :: V2 Int -> Int
manhattan = sum . fmap abs

solve1 :: [[Char]] -> Int
solve1 input =
  track grid start
    & cheats 2
    & filter (>= 100)
    & length
  where
    (grid, start) = readInput input

solve2 :: [[Char]] -> Int
solve2 input =
  track grid start
    & cheats 20
    & filter (>= 100)
    & length
  where
    (grid, start) = readInput input

main = do
  input <- readFile "inputs/Day20.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 1355
      solve2 (parse input) @?= 1007335
