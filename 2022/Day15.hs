module Day15 (main) where

import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl')
import Data.List qualified as L
import Data.Maybe (catMaybes, fromJust)
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

type Coord = V2 Int

data SensorInfo = SensorInfo
  { location :: Coord,
    nearestBeacon :: Coord
  }
  deriving (Eq, Show)

parse :: String -> [SensorInfo]
parse input = run $ sensorInfo `P.endBy` eol
  where
    sensorInfo =
      SensorInfo
        <$> (P.string "Sensor at " *> coord)
        <*> (P.string ": closest beacon is at " *> coord)
    coord =
      flip V2
        <$> (P.string "x=" *> signum)
        <*> (P.string ", y=" *> signum)
    -- Standard parsers
    signum = do
      sign <- P.option 1 ((-1) <$ P.char '-')
      number <- number
      return (sign * number)
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

manhattan :: V2 Int -> Int
manhattan (V2 y x) = abs y + abs x

rangeOnY :: Int -> SensorInfo -> Maybe (Int, Int)
rangeOnY testY si
  | xDist < 0 = Nothing
  | otherwise = Just (x si.location - xDist, x si.location + xDist)
  where
    xDist = manhattanRadius - yDist
    yDist = abs (y si.location - testY)
    manhattanRadius = manhattan (si.location - si.nearestBeacon)

y (V2 y _) = y

x (V2 _ x) = x

type Range = (Int, Int)

contains :: Range -> Range -> Bool
contains (x, y) (z, w) =
  z >= x && w <= y
    || x >= z && y <= w

overlap :: Range -> Range -> Bool
overlap a@(x, y) b@(z, w) =
  x >= z && x <= w
    || y >= z && y <= w
    || contains a b

mergeRanges :: [Range] -> [Range]
mergeRanges ranges =
  ranges
    & L.sort
    & foldl' f []
  where
    f [] range = [range]
    f (x : xs) range
      | overlap x range = merge x range : xs
      | otherwise = range : x : xs
    merge (x, y) (z, w) = (min x z, max y w)

rangeLength :: Range -> Int
rangeLength (x, y) = y - x + 1

solve1 :: Int -> [SensorInfo] -> Int
solve1 testY input = totalInRangeOnY - uniqueBeaconsOnY
  where
    uniqueBeaconsOnY =
      input
        <&> nearestBeacon
        & filter ((== testY) . y)
        & filter (\c -> any (overlap (x c, x c)) ranges)
        & L.nub
        & length
    ranges =
      input
        <&> rangeOnY testY
        & catMaybes
        & mergeRanges
    totalInRangeOnY =
      ranges
        <&> rangeLength
        & sum

solve2 :: Int -> [SensorInfo] -> Int
solve2 maxRange input =
  [0 .. maxRange]
    <&> (\y -> toCoord y (ranges y))
    & catMaybes
    & head
    & score
  where
    score (V2 y x) = x * 4000000 + y
    toCoord :: Int -> [Range] -> Maybe Coord
    toCoord y [(a, b)]
      | a > 0 = Just (V2 y (a - 1))
      | b < maxRange = Just (V2 y (b + 1))
      | otherwise = Nothing
    toCoord y ((a, _b) : _) = Just (V2 y (a - 1))
    toCoord _ _ = undefined

    ranges testY =
      input
        <&> rangeOnY testY
        & catMaybes
        & mergeRanges

main = do
  input <- readFile "inputs/Day15.txt"
  exampleInput <- readFile "inputs/Day15_example.txt"
  runTestTT $
    TestCase $ do
      solve1 10 (parse exampleInput) @?= 26
      solve1 2000000 (parse input) @?= 4424278
      solve2 20 (parse exampleInput) @?= 56000011
      solve2 4000000 (parse input) @?= 10382630753392
