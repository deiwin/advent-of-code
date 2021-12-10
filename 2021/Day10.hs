{-# LANGUAGE FlexibleContexts #-}

module Day10 (main) where

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
import Control.Monad (foldM)
import Data.Either (lefts, rights)

parse :: String -> _
parse = lines

solve1 :: _
solve1 input =
  input
    & fmap execute
    & lefts
    & fmap toScore
    & sum
  where
    toScore ']' = 57
    toScore ')' = 3
    toScore '}' = 1197
    toScore '>' = 25137
    toScore _ = undefined

execute :: String -> Either Char String
execute = foldM go ""
  where
    go :: String -> Char -> Either Char String
    go [] c
      | isOpenChar c = Right [c]
      | otherwise = Left c
    go s@(first:rest) c
      | isOpenChar c = Right (c:s)
      | first == toOpenChar c = Right rest
      | otherwise = Left c
    isOpenChar c = c `elem` ['[', '(', '{', '<']
    toOpenChar ']' = '['
    toOpenChar ')' = '('
    toOpenChar '}' = '{'
    toOpenChar '>' = '<'
    toOpenChar _ = undefined

solve2 :: [String] -> _
solve2 input =
  input
    & fmap execute
    & rights
    & fmap (score . fmap toCloseChar)
    & L.sort
    & middle
  where
    middle l = head $ drop (length l `div` 2) l
    score = foldl' score' 0
    score' acc c = (5 * acc) + toScore c
    toCloseChar '[' = ']'
    toCloseChar '(' = ')'
    toCloseChar '{' = '}'
    toCloseChar '<' = '>'
    toCloseChar _ = undefined
    toScore ')' = 1
    toScore ']' = 2
    toScore '}' = 3
    toScore '>' = 4
    toScore _ = undefined


main = do
  input <- readFile "inputs/Day10.txt"
  exampleInput <- readFile "inputs/Day10_example.txt"
  print $ solve2 $ parse input
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 26397
      solve1 (parse input) @?= 392139
      1 @?= 288957
