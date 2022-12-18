module Day18 (main) where

import Control.Monad.Memo (MonadMemo, memo, startEvalMemo, startRunMemo)
import Control.Applicative (empty, (<|>))
import Control.Arrow (first, second, (>>>))
import Control.Monad (guard, when)
import Criterion.Main
  ( bench,
    defaultMain,
    whnf,
  )
import Data.Array.IArray (Array)
import Data.Array.IArray qualified as A
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor (($>), (<$), (<&>))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
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
import Data.List qualified as L
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as M
import Data.Maybe
  ( catMaybes,
    fromJust,
    isJust,
  )
import Data.Ord (comparing)
import Data.Sequence
  ( Seq (..),
    (<|),
    (|>),
  )
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as VU
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

loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

withAirPockets :: Set Coord -> Set Coord
withAirPockets lava =
  bounds
    & range
    <&> (\c -> (c, isSurrounded c))
    & M.fromList
    & loeb
    & M.filter id
    & M.keys
    & S.fromList
  where
    isSurrounded !c !m
      | c `S.member` lava = True
      | not (all (inRange bounds) (neighbors c)) = False
      | otherwise = all (m M.!) (neighbors c)
    bounds =
      ( V3 (minimum (x <$> lavaList)) (minimum (y <$> lavaList)) (minimum (z <$> lavaList)),
        V3 (maximum (x <$> lavaList)) (maximum (y <$> lavaList)) (maximum (z <$> lavaList))
      )
      where
        lavaList = S.toList lava
        x (V3 x _ _) = x
        y (V3 _ y _) = y
        z (V3 _ _ z) = z

solve2 :: [Coord] -> _
solve2 input =
  input
    & S.fromList
    & withAirPockets
    & exposedSideCount

main = do
  input <- readFile "inputs/Day18.txt"
  exampleInput <- readFile "inputs/Day18_example.txt"
  print $ solve2 $ parse input
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 64
      solve1 (parse input) @?= 4548
      -- solve2 (parse exampleInput) @?= 58
      -- solve2 (parse input) @?= 58
