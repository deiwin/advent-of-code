module Day20 (main) where

import Control.Arrow (second)
import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List (foldl')
import Data.List qualified as L
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [Int]
parse input = read <$> lines input

mix :: Int -> [Int] -> IntMap Int
mix n input =
  input
    & zip [0 ..]
    -- & foldl' f (indexMap, indexMap)
    -- & repeat
    & (\xs -> L.iterate' (\s -> foldl' f s xs) (indexMap, indexMap))
    & (L.!! n)
    & snd
    <&> (initialMap IM.!)
  where
    -- Tried a faster option but couldn't get it to work
    -- repeat (iToC, cToI) = (L.!! (n - 1)) $ L.iterate' (IM.mapKeys (iToC IM.!)) cToI
    f (iToC, cToI) (i, x) = (newIToC, newCToI)
      where
        current i = iToC IM.! i
        newIToC =
          cToIUpdates
            <&> swap
            & IM.fromList
            & flip IM.union iToC
        newCToI =
          cToIUpdates
            & IM.fromList
            & flip IM.union cToI
        cToIUpdates = (\(fromC, toC) -> (toC, cToI IM.! fromC)) <$> toUpdate
        toUpdate =
          iDeltas
            <&> second (+ updateDelta) . (\i -> (i, i)) . (+ current i)
            & ((current i, current i + x') :)
            <&> bothmap wrap
          where
            x' = wrap' x
            updateDelta
              | x' == 0 = 0
              | x' > 0 = -1
              | otherwise = 1
            iDeltas
              | x' == 0 = []
              | otherwise = [1 .. x']
    wrap' x = ((x - 1) `mod` (size - 1)) + 1
    wrap = (`mod` size)
    size = length input
    initialMap = IM.fromList (zip [0 ..] input)
    indexMap = IM.fromList (take size $ zip [0 ..] [0 ..])
    bothmap f = bimap f f

score :: IntMap Int -> Int
score m =
  [1000, 2000, 3000]
    <&> (m IM.!) . wrap . (+ zeroIndex)
    & sum
  where
    wrap = (`mod` IM.size m)
    zeroIndex =
      IM.toList m
        & L.find ((== 0) . snd)
        & fromJust
        & fst

solve1 :: [Int] -> Int
solve1 input =
  input
    & mix 1
    & score

solve2 :: [Int] -> Int
solve2 input =
  input
    <&> (* 811589153)
    & mix 10
    & score

main = do
  input <- readFile "inputs/Day20.txt"
  exampleInput <- readFile "inputs/Day20_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 3
      solve1 (parse input) @?= 7004
      solve2 (parse exampleInput) @?= 1623178306
      solve2 (parse input) @?= 17200008919529
