module Day16 (main) where

import Data.Array.IArray qualified as A
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Graph.Inductive.Graph (Node, lab, match, mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (spLength)
import Data.IntSet qualified as IS
import Data.Ix (inRange, range)
import Data.List (foldl')
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
import Data.PQueue.Min (MinQueue (..))
import Data.PQueue.Min qualified as PQ
import Data.Set qualified as S
import Data.Tuple (swap)
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [[Char]]
parse = lines

type Coord = V2 Int

type Grid = A.Array Coord Cell

data Cell = Wall | Start | End | Open
  deriving (Show, Eq)

data Dir = N | E | S | W
  deriving (Show, Eq, Ord, Bounded, Enum)

dirs :: [Dir]
dirs = [minBound .. maxBound]

moveDiff :: Dir -> Coord
moveDiff = \case
  N -> V2 (-1) 0
  S -> V2 1 0
  E -> V2 0 1
  W -> V2 0 (-1)

counterClockwise :: (Eq a, Enum a, Bounded a) => a -> a
counterClockwise x = if x == minBound then maxBound else pred x

clockwise :: (Eq a, Enum a, Bounded a) => a -> a
clockwise x = if x == maxBound then minBound else succ x

type NLabel = (Coord, Dir)

type ELabel = Int

type NMap = Map NLabel Node

type G = Gr NLabel ELabel

readInput :: [[Char]] -> (G, Node, [Node])
readInput rows = (graph, startNode, endNodes)
  where
    graph :: G
    graph = mkGraph nodes edges
    nodes :: [(Node, NLabel)]
    nodes =
      range bounds
        & filter ((/= Wall) . (grid A.!))
        & concatMap localNodes
        & zip [0 ..]
    edges :: [(Node, Node, ELabel)]
    edges =
      range bounds
        & filter ((/= Wall) . (grid A.!))
        & concatMap localEdges
    nodeMap :: NMap
    nodeMap = M.fromList (swap <$> nodes)
    startNode = nodeMap M.! (startC, startDir)
    endNodes = (nodeMap M.!) . (endC,) <$> dirs
    startDir = E
    startC =
      range bounds
        & L.find ((== Start) . (grid A.!))
        & fromJust
    endC =
      range bounds
        & L.find ((== End) . (grid A.!))
        & fromJust
    localNodes c =
      dirs
        <&> (c,)
    localEdges c =
      dirs
        & L.concatMap
          ( \dir ->
              [ ((c, dir), (c, clockwise dir), 1000),
                ((c, dir), (c, counterClockwise dir), 1000),
                ((c, dir), (c + moveDiff dir, dir), 1)
              ]
                & filter (\(_, (c', _), _) -> inRange bounds c' && grid A.! c' /= Wall)
                <&> \(n1, n2, w) -> (nodeMap M.! n1, nodeMap M.! n2, w)
          )
    grid :: Grid
    grid = A.listArray bounds $ toCell <$> concat rows
    bounds = (V2 0 0, V2 (length rows - 1) (length (head rows) - 1))
    toCell = \case
      '#' -> Wall
      'S' -> Start
      'E' -> End
      '.' -> Open
      _ -> error "Invalid cell"

solve1 :: [[Char]] -> Int
solve1 input =
  endNodes
    & mapMaybe (\endNode -> spLength startNode endNode graph)
    & minimum
  where
    (graph, startNode, endNodes) = readInput input

type Queue = MinQueue (Int, [Node])

type Visited = Map Node Int

type Acc = (Queue, Visited)

solve2 :: [[Char]] -> Int
solve2 input =
  L.unfoldr go (PQ.singleton (0, [startNode]), M.empty)
    & catMaybes
    <&> IS.fromList
    & foldl' IS.union IS.empty
    & IS.toList
    & mapMaybe (lab graph)
    <&> fst
    & S.fromList
    & S.size
  where
    (graph, startNode, endNodes) = readInput input
    go :: Acc -> Maybe (Maybe [Node], Acc)
    go (Empty, _) = Nothing
    go ((_, []) :< _, _) = undefined
    go ((cost, ns@(n : _)) :< xs, visited)
      | n `elem` endNodes = Just (Just ns, (xs, visited))
      | fromMaybe maxBound (visited M.!? n) < cost = Just (Nothing, (xs, visited))
      | otherwise = Just (Nothing, (newXs, newVisited))
      where
        newVisited = M.insert n cost visited
        newXs = PQ.fromList news <> xs
        news =
          case match n graph of
            (Nothing, _) -> []
            (Just (_, _, _, out), _) -> (\(w, n') -> (cost + w, n' : ns)) <$> out

main = do
  input <- readFile "inputs/Day16.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 78428
      solve2 (parse input) @?= 463
