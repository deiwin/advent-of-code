module Day11 (main) where

import Control.Arrow (second, (>>>))
import Control.Monad (liftM2)
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import Data.Function ((&))
import Data.Ix (inRange, rangeSize)
import qualified Data.List as L
import Data.Maybe (fromMaybe, isNothing)
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

type Octopus = Maybe Int

type Grid = Array (V2 Int) Octopus

parse :: String -> [[Int]]
parse = fmap (fmap (read . (: []))) . lines

solve1 :: [[Int]] -> Int
solve1 =
  buildGrid
    >>> L.unfoldr (Just . playRound)
    >>> take 100
    >>> sum

solve2 :: [[Int]] -> Maybe Int
solve2 input =
  array
    & L.unfoldr (Just . playRound)
    & L.elemIndex (rangeSize (A.bounds array))
    & fmap (+ 1)
  where
    array = buildGrid input

playRound :: Grid -> (Int, Grid)
playRound arr = (resetCount, resetArr)
  where
    resetCount =
      flashedArr
        & A.assocs
        & filter (isNothing . snd)
        & length
    resetArr :: Grid
    resetArr = A.amap (Just . fromMaybe 0) flashedArr
    flashedArr :: Grid
    flashedArr = converge flash increasedArr
    flash :: Grid -> Grid
    flash arr =
      arr
        & (A.// toFlash)
        & flip (A.accum (liftM2 (+))) toIncrease
      where
        toFlash :: [(V2 Int, Octopus)]
        toFlash =
          A.assocs arr
            & filter (maybe False (> 9) . snd)
            & fmap (second (const Nothing))
        toIncrease :: [(V2 Int, Octopus)]
        toIncrease =
          toFlash
            & concatMap (surrounding arr . fst)
            & flip zip (repeat (Just 1))
    increasedArr :: Grid
    increasedArr = A.amap (fmap (+ 1)) arr

surrounding :: Grid -> V2 Int -> [V2 Int]
surrounding arr coord =
  [V2 y x | x <- [-1, 0, 1], y <- [-1, 0, 1], x /= 0 || y /= 0]
    & fmap (+ coord)
    & filter (inRange (A.bounds arr))

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

buildGrid :: [[Int]] -> Grid
buildGrid input = A.listArray bounds (Just <$> concat input)
  where
    bounds = (V2 0 0, V2 (length input - 1) (length (head input) - 1))

main = do
  input <- readFile "inputs/Day11.txt"
  exampleInput <- readFile "inputs/Day11_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 1656
      solve1 (parse input) @?= 1661
      solve2 (parse exampleInput) @?= Just 195
      solve2 (parse input) @?= Just 334
