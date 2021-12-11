module Day11 (main) where

import Control.Arrow (second)
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
solve1 input =
  (0, array)
    & iterate (playRound . snd)
    & drop 1
    & take 100
    & fmap fst
    & sum
  where
    array :: Array (V2 Int) Octopus
    array = A.listArray bounds (Just <$> concat input)
    bounds = (V2 0 0, V2 (length input - 1) (length (head input) - 1))

solve2 :: [[Int]] -> Maybe Int
solve2 input =
  (0, array)
    & iterate (playRound . snd)
    & L.findIndex ((== rangeSize bounds) . fst)
  where
    array :: Array (V2 Int) Octopus
    array = A.listArray bounds (Just <$> concat input)
    bounds = (V2 0 0, V2 (length input - 1) (length (head input) - 1))

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
  [ V2 (-1) 0,
    V2 1 0,
    V2 0 (-1),
    V2 0 1,
    V2 1 1,
    V2 (-1) 1,
    V2 1 (-1),
    V2 (-1) (-1)
  ]
    & fmap (+ coord)
    & filter (inRange (A.bounds arr))

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

main = do
  input <- readFile "inputs/Day11.txt"
  exampleInput <- readFile "inputs/Day11_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 1656
      solve1 (parse input) @?= 1661
      solve2 (parse exampleInput) @?= Just 195
      solve2 (parse input) @?= Just 334
