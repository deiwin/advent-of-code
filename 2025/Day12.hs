module Day12 (main) where

import Control.Applicative ((<|>))
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Ix (range, rangeSize)
import Data.List qualified as L
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

data Shape = Shape
  { bounds :: (V2 Int, V2 Int),
    points :: Set (V2 Int)
  }
  deriving (Show)

data Region = Region
  { bounds :: (V2 Int, V2 Int),
    shapeCounts :: [Int]
  }
  deriving (Show)

parse :: String -> ([Shape], [Region])
parse input = run $ do
  shapes <- shape `P.sepBy1` eol <* eol
  regions <- region `P.endBy1` eol
  return (shapes, regions)
  where
    shape = do
      number <* P.char ':' <* eol
      rows <- P.many1 (P.char '.' <|> P.char '#') `P.endBy1` eol
      let bounds = (V2 0 0, V2 (length rows - 1) (length (head rows) - 1))
      let points =
            concat rows
              & zip (range bounds)
              & filter ((== '#') . snd)
              <&> fst
              & S.fromList
      return (Shape {..})
    region = do
      width <- number <* P.char 'x'
      length <- number <* P.string ": "
      let bounds = (V2 0 0, V2 (length - 1) (width - 1))
      shapeCounts <- number `P.sepBy1` P.char ' '
      return (Region {..})
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

fit :: [Shape] -> Region -> Bool
fit shapes region = sum (zipWith (*) shapeSizes region.shapeCounts) <= rangeSize region.bounds
  where
    shapeSizes = S.size . points <$> shapes

solve :: ([Shape], [Region]) -> Int
solve (shapes, regions) =
  regions
    & filter (fit shapes)
    & length

main = do
  input <- readFile "inputs/Day12.txt"
  runTestTT $
    TestCase $ do
      solve (parse input) @?= 410
