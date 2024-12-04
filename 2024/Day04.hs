module Day04 (main) where

import Data.Array.IArray qualified as A
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Ix (inRange, range)
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [[Char]]
parse = lines

type Coord = V2 Int

type Grid = A.Array Coord Char

createGrid :: [[Char]] -> Grid
createGrid rows = A.listArray bounds $ concat rows
  where
    bounds = (V2 0 0, V2 (length rows - 1) (length (head rows) - 1))

type Path = [V2 Int]

xmasPaths :: [Path]
xmasPaths =
  [ V2 0 1, -- right
    V2 1 1, -- down right
    V2 1 0, -- down
    V2 1 (-1), -- down left
    V2 0 (-1), -- left
    V2 (-1) (-1), -- up left
    V2 (-1) 0, -- up
    V2 (-1) 1 -- up right
  ]
    <&> \dir -> take 4 $ iterate (+ dir) (V2 0 0)

crossMAS :: Path
crossMAS =
  [ V2 (-1) (-1), -- up left
    V2 0 0, -- center
    V2 1 1, -- down right
    V2 (-1) 1, -- up right
    V2 0 0, -- center
    V2 1 (-1) -- down left
  ]

cartProd :: [a] -> [a] -> [(a, a)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

selfCartProd :: [a] -> [(a, a)]
selfCartProd xs = cartProd xs xs

solve1 :: [[Char]] -> Int
solve1 input =
  candidates
    & filter (all (inRange bounds))
    & filter isXMAS
    & length
  where
    grid = createGrid input
    bounds = A.bounds grid
    xCoords = filter (\coord -> grid A.! coord == 'X') $ range bounds
    candidates = (\coord -> fmap (coord +) <$> xmasPaths) `concatMap` xCoords
    isXMAS cs = ((grid A.!) <$> cs) == "XMAS"

solve2 :: [[Char]] -> Int
solve2 input =
  candidates
    & filter (all (inRange bounds))
    & filter isCrossMAS
    & length
  where
    grid = createGrid input
    bounds = A.bounds grid
    aCoords = filter (\coord -> grid A.! coord == 'A') $ range bounds
    candidates = (\coord -> (coord +) <$> crossMAS) <$> aCoords
    isCrossMAS cs = ((grid A.!) <$> cs) `elem` crossMASCombinations
    crossMASCombinations = uncurry (++) <$> selfCartProd ["MAS", "SAM"]

main = do
  input <- readFile "inputs/Day04.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 2406
      solve2 (parse input) @?= 1807
