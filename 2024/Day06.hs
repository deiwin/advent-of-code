module Day06 (main) where

import Control.Monad (foldM)
import Data.Array.IArray qualified as A
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Ix (inRange, range)
import Data.List qualified as L
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set)
import Data.Set qualified as S
import Linear.V2 (V2 (..), perp)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

data Cell = Free | Occupied
  deriving (Show, Eq)

parse :: String -> [[Char]]
parse = lines

type Coord = V2 Int

type Dir = V2 Int

type Grid = A.Array Coord Cell

readInput :: [[Char]] -> (Grid, (Coord, Dir))
readInput rows = (grid, state)
  where
    grid = A.listArray bounds $ f <$> concat rows
    state =
      zip (concat rows) (range bounds)
        & L.find ((`elem` ("<>^v" :: [Char])) . fst)
        & fromJust
        & \(c, coord) ->
          ( coord,
            case c of
              '>' -> V2 0 1
              '<' -> V2 0 (-1)
              '^' -> V2 (-1) 0
              'v' -> V2 1 0
              _ -> undefined
          )
    bounds = (V2 0 0, V2 (length rows - 1) (length (head rows) - 1))
    f c | c `elem` (".<>^v" :: [Char]) = Free
    f '#' = Occupied
    f _ = undefined

step :: Grid -> (Coord, Dir) -> Maybe (Coord, Dir)
step grid (coord, dir) =
  if inRange (A.bounds grid) nextCoord
    then Just (newCoord, newDir)
    else Nothing
  where
    nextCoord = coord + dir
    nextCell = grid A.! nextCoord
    (newCoord, newDir) =
      case nextCell of
        Free -> (nextCoord, dir)
        Occupied -> (coord, turn dir)

turn :: Dir -> Dir
turn = perp . perp . perp

solve1 :: [[Char]] -> Int
solve1 input =
  initialState
    & L.unfoldr (fmap (\x -> (x, x)) . step grid)
    <&> fst
    & S.fromList
    & S.size
  where
    (grid, initialState) = readInput input

loops :: Grid -> (Coord, Dir) -> Set (Coord, Dir) -> Bool
loops grid s visited = isNothing $ foldM f visited nextStates
  where
    nextStates = L.unfoldr (fmap (\x -> (x, x)) . step grid) s
    f visited s = if s `S.member` visited then Nothing else Just $ S.insert s visited

solve2 :: [[Char]] -> Int
solve2 input =
  range (A.bounds grid)
    & filter (\c -> grid A.! c == Free && c /= initialCoord)
    <&> (\c -> grid A.// [(c, Occupied)])
    & filter (\g -> loops g initialState S.empty)
    & length
  where
    (grid, initialState@(initialCoord, _)) = readInput input

main = do
  input <- readFile "inputs/Day06.txt"
  exampleInput <- readFile "inputs/Day06_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 4964
      solve2 (parse exampleInput) @?= 6
      solve2 (parse input) @?= 1740
