module Day09 (main) where

import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

type Coord = V2 Int

parse :: String -> [Coord]
parse input = run $ coord `P.endBy1` eol
  where
    coord = do
      x <- number <* P.char ','
      y <- number
      return (V2 x y)
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

pairs :: [a] -> [(a, a)]
pairs xs = concatMap (\(x, ys) -> (x,) <$> ys) $ zip xs (tail $ L.tails xs)

squareSize :: Coord -> Coord -> Int
squareSize a b = let (V2 x y) = a - b in (abs x + 1) * (abs y + 1)

solve1 :: [Coord] -> Int
solve1 input =
  input
    & pairs
    <&> uncurry squareSize
    & maximum

consPairs :: [a] -> [(a, a)]
consPairs xs = zip xs (tail xs) ++ [(last xs, head xs)]

cuts :: (Coord, Coord) -> (Coord, Coord) -> Bool
cuts (V2 l1X l1Y, V2 l2X l2Y) (V2 sq1X sq1Y, V2 sq2X sq2Y) =
  horizontalCut || verticalCut
  where
    horizontalCut =
      horizontalLine
        && l1Y > sqMinY
        && l1Y < sqMaxY
        && lMinX < sqMaxX
        && lMaxX > sqMinY
    verticalCut =
      verticalLine
        && l1X > sqMinX
        && l1X < sqMaxX
        && lMinY < sqMaxY
        && lMaxY > sqMinY
    sqMinX = min sq1X sq2X
    sqMinY = min sq1Y sq2Y
    sqMaxX = max sq1X sq2X
    sqMaxY = max sq1Y sq2Y
    lMinX = min l1X l2X
    lMinY = min l1Y l2Y
    lMaxX = max l1X l2X
    lMaxY = max l1Y l2Y
    horizontalLine = l1Y == l2Y
    verticalLine = l1X == l2X

solve2 :: [Coord] -> Int
solve2 input =
  input
    & pairs
    & filter (\sq -> not $ any (`cuts` sq) borders)
    <&> uncurry squareSize
    & maximum
  where
    borders = consPairs input

main = do
  input <- readFile "inputs/Day09.txt"
  exampleInput <- readFile "inputs/Day09_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 50
      solve1 (parse input) @?= 4725826296
      solve2 (parse exampleInput) @?= 24
      solve2 (parse input) @?= 1637556834
