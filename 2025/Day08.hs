module Day08 (main) where

import Control.Arrow (second)
import Data.Array.IArray qualified as A
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Graph as G
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.Maybe (fromJust)
import Linear.Metric (distance)
import Linear.V3 (V3 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

type Coord = V3 Int

parse :: String -> [Coord]
parse input = run (coord `P.endBy1` eol)
  where
    coord = do
      x <- number <* P.char ','
      y <- number <* P.char ','
      z <- number
      return (V3 x y z)
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

pairs :: [a] -> [(a, a)]
pairs xs = concatMap (\(x, ys) -> (x,) <$> ys) $ zip xs (tail $ L.tails xs)

solve1 :: Int -> [Coord] -> Int
solve1 n nodes =
  G.components graph
    <&> length
    & L.sort
    & reverse
    & take 3
    & product
  where
    distances :: [(Coord, Coord, Float)]
    distances =
      nodes
        & pairs
        <&> ((\(a, b) -> (a, b, distance (fromIntegral <$> a) (fromIntegral <$> b))))
    nodeMap :: HashMap Coord Int
    nodeMap = HM.fromList $ zip nodes [0 ..]
    bounds :: Bounds
    bounds = (0, length nodes - 1)
    edges :: [Edge]
    edges =
      distances
        & L.sortOn thd3
        & take n
        <&> both (nodeMap HM.!) . (\(a, b, _) -> (a, b))
    graph :: Graph
    graph = G.buildG bounds edges

solve2 :: [Coord] -> Int
solve2 nodes =
  zip distances graphs
    <&> second componentCount
    & takeWhile ((/= 1) . snd)
    & last
    & score
  where
    distances :: [(Coord, Coord, Float)]
    distances =
      nodes
        & pairs
        <&> (\(a, b) -> (a, b, distance (fromIntegral <$> a) (fromIntegral <$> b)))
        & L.sortOn thd3
    nodeMap :: HashMap Coord Int
    nodeMap = HM.fromList $ zip nodes [0 ..]
    bounds :: Bounds
    bounds = (0, length nodes - 1)
    initialGraph :: Graph
    initialGraph = G.buildG bounds []
    graphs =
      distances
        <&> both (nodeMap HM.!) . (\(a, b, _) -> (a, b))
        & scanl addEdge initialGraph
    addEdge g (a, b) = A.accum (++) g [(a, [b]), (b, [a])]
    componentCount = length . G.dff
    score ((V3 x1 _ _, V3 x2 _ _, _), _) = x1 * x2

main = do
  input <- readFile "inputs/Day08.txt"
  exampleInput <- readFile "inputs/Day08_example.txt"
  runTestTT $
    TestCase $ do
      solve1 10 (parse exampleInput) @?= 40
      solve1 1000 (parse input) @?= 97384
      solve2 (parse exampleInput) @?= 25272
      solve2 (parse input) @?= 9003685096
