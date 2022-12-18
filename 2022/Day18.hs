module Day18 (main) where

import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Ix (inRange, range)
import Data.List (foldl')
import Data.List qualified as L
import Data.Maybe (fromJust)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Linear.V3 (V3 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

type Coord = V3 Int

parse :: String -> [Coord]
parse input = run $ coord `P.endBy` eol
  where
    coord =
      V3
        <$> (number <* P.char ',')
        <*> (number <* P.char ',')
        <*> number
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

neighbors :: Coord -> [Coord]
neighbors c =
  ((\x -> V3 x 0 0) <$> [1, -1])
    ++ ((\y -> V3 0 y 0) <$> [1, -1])
    ++ (V3 0 0 <$> [1, -1])
    <&> (+ c)

exposedSideCount :: Set Coord -> Int
exposedSideCount lava =
  lava
    & S.toList
    <&> exposedSides
    & sum
  where
    exposedSides c =
      neighbors c
        & filter (not . (`S.member` lava))
        & length

solve1 :: [Coord] -> Int
solve1 input =
  input
    & S.fromList
    & exposedSideCount

withAirPockets :: Set Coord -> Set Coord
withAirPockets lava =
  bounds
    & range
    & foldl' f lava
  where
    f lava c = S.union lava (isBubble lava c)
    bounds = lavaBounds lava

isBubble :: Set Coord -> Coord -> Set Coord
isBubble lava c = go c Seq.empty (S.singleton c)
  where
    go :: Coord -> Seq Coord -> Set Coord -> Set Coord
    go c toVisit explored
      | not (inRange bounds c) = S.empty
      | otherwise =
          case newToVisit of
            Empty -> explored
            x :<| xs -> go x xs newExplored
      where
        newToVisit =
          newToExplore
            & Seq.fromList
            & (toVisit Seq.><)
        newToExplore =
          neighbors c
            & filter (not . (`S.member` explored))
            & filter (not . (`S.member` lava))
        newExplored = S.union explored (S.fromList newToExplore)
    bounds = lavaBounds lava

lavaBounds :: Set Coord -> (Coord, Coord)
lavaBounds lava =
  ( V3 (minimum (x <$> lavaList)) (minimum (y <$> lavaList)) (minimum (z <$> lavaList)),
    V3 (maximum (x <$> lavaList)) (maximum (y <$> lavaList)) (maximum (z <$> lavaList))
  )
  where
    lavaList = S.toList lava
    x (V3 x _ _) = x
    y (V3 _ y _) = y
    z (V3 _ _ z) = z

solve2 :: [Coord] -> Int
solve2 input =
  input
    & S.fromList
    & withAirPockets
    & exposedSideCount

main = do
  input <- readFile "inputs/Day18.txt"
  exampleInput <- readFile "inputs/Day18_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 64
      solve1 (parse input) @?= 4548
      solve2 (parse exampleInput) @?= 58
      solve2 (parse input) @?= 2588
