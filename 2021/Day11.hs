{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}

module Day11 (main) where

import Control.Applicative (empty, (<|>))
import Control.Arrow (second, (>>>))
import Control.Monad (guard)
import Criterion.Main
  ( bench,
    defaultMain,
    whnf,
  )
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import qualified Data.Char as C
import Data.Function ((&))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Ix
  ( inRange,
    range,
  )
import Data.List
  ( foldl',
    foldl1',
    isPrefixOf,
    iterate,
  )
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
  ( catMaybes,
    fromJust,
    isJust,
    maybe,
    fromMaybe,
    isNothing
  )
import Data.Ord (comparing)
import Data.Sequence
  ( Seq (..),
    (<|),
    (|>),
  )
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import Data.Void (Void)
import Debug.Trace
  ( traceShow,
    traceShowId,
  )
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P
import Control.Arrow (second)
import Control.Monad (liftM2)

type Octopus = Maybe Int
type Grid = Array (V2 Int) Octopus

parse :: String -> [[Int]]
parse = lines >>> fmap (fmap (read . (: [])))

solve1 :: _
solve1 input = fst $ playNRounds 100 array
  where
    playNRounds n arr = foldl' go (0, arr) [1..n]
      where
        go (x, arr) _ = case playRound arr of
                          (y, newArr) -> (x + y, newArr)
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
                & concatMap (surrounding . fst)
                & flip zip (repeat (Just 1))
        increasedArr =
          range (A.bounds arr)
            & flip zip (repeat (Just 1))
            & A.accum (liftM2 (+)) arr
    surrounding coord =
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
        & filter (inRange bounds)
    array :: Array (V2 Int) Octopus
    array = A.listArray bounds (Just <$> concat input)
    bounds = (V2 0 0, V2 (length input - 1) (length (head input) - 1))

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

main = do
  input <- readFile "inputs/Day11.txt"
  exampleInput <- readFile "inputs/Day11_example.txt"
  print $ solve1 $ parse input
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 1656
      solve1 (parse input) @?= 1661
      1 @?= 1
