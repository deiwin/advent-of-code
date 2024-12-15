module Day15 (main) where

import Control.Arrow (first)
import Control.Monad (foldM)
import Data.Array.IArray qualified as A
import Data.Char qualified as C
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.Ix (inRange, range)
import Data.List (foldl')
import Data.List qualified as L
import Data.Maybe (fromJust)
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import Data.Set qualified as S
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

parse :: String -> ([[Char]], String)
parse input = run $ do
  grid <- P.many1 char `P.endBy1` eol <* eol
  moves <- concat <$> P.many1 char `P.endBy1` eol
  return (grid, moves)
  where
    char = P.satisfy (not . C.isSpace)
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

type Coord = V2 Int

type Grid = A.Array Coord Cell

data Cell = Wall | BoxL | BoxR | Robot | Open
  deriving (Eq, Show, Ord)

type Move = V2 Int

readInput :: ([[Char]], String) -> (Grid, [Move])
readInput (gridInput, movesInput) = (grid, moves)
  where
    grid = A.listArray bounds $ toCell <$> concat gridInput
    moves = toMove <$> movesInput
    bounds = (V2 0 0, V2 (length gridInput - 1) (length (head gridInput) - 1))
    toCell = \case
      '#' -> Wall
      'O' -> BoxL
      '@' -> Robot
      '.' -> Open
      _ -> error "unknown cell"
    toMove = \case
      '^' -> V2 (-1) 0
      'v' -> V2 1 0
      '>' -> V2 0 1
      '<' -> V2 0 (-1)
      _ -> error "unknown move"

move :: Grid -> Move -> Grid
move grid move = newGrid
  where
    robotC =
      range (A.bounds grid)
        & L.find (\c -> grid A.! c == Robot)
        & fromJust
    newGrid =
      case firstOpen of
        Just c
          | c == robotC + move -> grid A.// [(robotC, Open), (c, Robot)]
          | otherwise -> grid A.// [(robotC, Open), (robotC + move, Robot), (c, BoxL)]
        Nothing -> grid
    firstOpen =
      robotC
        & iterate (+ move)
        & tail
        & L.takeWhile (\c -> inRange bounds c && grid A.! c /= Wall)
        & L.find (\c -> grid A.! c == Open)
    bounds = A.bounds grid

gps :: Coord -> Int
gps (V2 y x) = y * 100 + x

solve1 :: ([[Char]], String) -> Int
solve1 input =
  range bounds
    & filter (\c -> finalGrid A.! c == BoxL)
    <&> gps
    & sum
  where
    finalGrid = foldl' move grid moves
    (grid, moves) = readInput input
    bounds = A.bounds grid

grow :: Grid -> Grid
grow grid = A.array newBounds newAssocs
  where
    (lower, V2 uy ux) = A.bounds grid
    newBounds = (lower, V2 uy (((ux + 1) * 2) - 1))
    newAssocs = do
      (c, cell) <- A.assocs grid
      let newCell = case cell of
            Wall -> Wall
            BoxL -> BoxR
            _ -> Open
      let newC = c * V2 1 2
      [(newC, cell), (newC + V2 0 1, newCell)]

move' :: Grid -> Move -> Grid
move' grid move = newGrid
  where
    robotC =
      range (A.bounds grid)
        & L.find (\c -> grid A.! c == Robot)
        & fromJust
    newGrid = case push robotC of
      Just assocs -> grid A.// assocs
      Nothing -> grid
    push c = L.sortBy (comparing (Down . snd)) . fst <$> push' c S.empty
    push' :: Coord -> Set Coord -> Maybe ([(Coord, Cell)], Set Coord)
    push' c visited = first (((c, Open) :) . ((nextC, cell) :)) <$> recPush
      where
        recPush :: Maybe ([(Coord, Cell)], Set Coord)
        recPush
          | nextCell == Wall = Nothing
          | c `S.member` visited = Just ([], nextVisited)
          | cell == nextCell && cell `elem` [BoxL, BoxR] && verticalMove = push' nextC nextVisited
          | nextCell == BoxR && verticalMove = pushM [nextC, nextC + V2 0 (-1)]
          | nextCell == BoxL && verticalMove = pushM [nextC, nextC + V2 0 1]
          | nextCell `elem` [BoxR, BoxL] = push' nextC nextVisited
          | nextCell == Open = Just ([], nextVisited)
          | otherwise = error "unknown push' scenario"
        pushM = foldM (\(as, vs) nc -> first (++ as) <$> push' nc vs) ([], visited)
        nextVisited = S.insert c visited
        cell = grid A.! c
        nextCell = grid A.! nextC
        nextC = c + move
    verticalMove = move `elem` [V2 1 0, V2 (-1) 0]

solve2 :: ([[Char]], String) -> Int
solve2 input =
  range bounds
    & filter (\c -> finalGrid A.! c == BoxL)
    <&> gps
    & sum
  where
    finalGrid = foldl' move' newGrid moves
    newGrid = grow grid
    (grid, moves) = readInput input
    bounds = A.bounds newGrid

-- Debugging

showGrid :: Grid -> String
showGrid grid =
  range (A.bounds grid)
    & L.groupBy ((==) `on` y)
    <&> fmap toChar
    & unlines
  where
    toChar c = case grid A.! c of
      Wall -> '#'
      BoxL -> '['
      BoxR -> ']'
      Robot -> '@'
      Open -> '.'
    y (V2 y _) = y

broken :: Grid -> Bool
broken grid =
  range (A.bounds grid)
    & any
      ( \c ->
          (grid A.! c == BoxL && grid A.! (c + V2 0 1) /= BoxR)
            || (grid A.! c == BoxR && grid A.! (c + V2 0 (-1)) /= BoxL)
      )

main = do
  input <- readFile "inputs/Day15.txt"
  exampleInput <- readFile "inputs/Day15_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 10092
      solve1 (parse input) @?= 1499739
      solve2 (parse exampleInput) @?= 9021
      solve2 (parse input) @?= 1522215
