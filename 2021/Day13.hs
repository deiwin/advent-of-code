module Day13 (main) where

import qualified Data.Char as C
import Data.Function ((&))
import Data.Ix (range)
import Data.List (foldl')
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P

data Dim = X | Y
  deriving (Eq, Read)

parse :: String -> ([V2 Int], [(Dim, Int)])
parse input = run $ do
  dots <- dot `P.endBy1` eol <* eol
  folds <- fold `P.endBy1` eol
  P.eof
  return (dots, folds)
  where
    fold = do
      P.string "fold along "
      dim <- read . (: []) . C.toUpper <$> letter
      a <- P.char '=' *> number
      return (dim, a)
    -- dim = `
    dot = do
      x <- number <* P.char ','
      y <- number
      return (V2 y x)
    -- Standard parsers
    letter = P.satisfy C.isLetter
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: ([V2 Int], [(Dim, Int)]) -> Int
solve1 input = S.size $ fold dotSet (head (snd input))
  where
    dotSet = S.fromList (fst input)

solve2 :: ([V2 Int], [(Dim, Int)]) -> Set (V2 Int)
solve2 input = foldl' fold dotSet (snd input)
  where
    dotSet = S.fromList (fst input)

fold :: Set (V2 Int) -> (Dim, Int) -> Set (V2 Int)
fold dotSet (X, fp) = S.map (\(V2 y x) -> V2 y (foldWith fp x)) dotSet
fold dotSet (Y, fp) = S.map (\(V2 y x) -> V2 (foldWith fp y) x) dotSet

foldWith :: Int -> Int -> Int
foldWith foldPoint real
  | real > foldPoint = foldPoint - (real - foldPoint)
  | otherwise = real

showGrid :: Set (V2 Int) -> String
showGrid dotSet =
  range bounds
    & L.groupBy equalYs
    & fmap (fmap showDot)
    & unlines
  where
    showDot c
      | c `S.member` dotSet = '#'
      | otherwise = ' '
    equalYs (V2 y1 _) (V2 y2 _) = y1 == y2
    bounds = (V2 0 0, V2 yMax xMax)
      where
        dotList = S.toList dotSet
        xMax = maximum (getX <$> dotList)
        yMax = maximum (getY <$> dotList)
        getX (V2 _ x) = x
        getY (V2 y _) = y

main = do
  input <- readFile "inputs/Day13.txt"
  exampleInput <- readFile "inputs/Day13_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 17
      solve1 (parse input) @?= 704
  putStrLn $ showGrid $ solve2 $ parse exampleInput
  putStrLn $ showGrid $ solve2 $ parse input
