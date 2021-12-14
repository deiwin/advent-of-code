{-# LANGUAGE FlexibleContexts #-}

module Day14 (main) where

import Control.Applicative (empty, (<|>))
import Control.Arrow (first, second, (>>>))
import Control.Monad (guard)
import Control.Monad.Memo (MonadMemo, for2, memo, runMemo)
import Criterion.Main
  ( bench,
    defaultMain,
    whnf,
  )
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import qualified Data.Char as C
import Data.Function (fix, (&))
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

type Rule = ((Char, Char), Char)

type Rules = Map (Char, Char) Char

type Cache = Map (Int, (Char, Char)) (Map Char Integer)


parse :: String -> (String, [Rule])
parse input = run $ do
  template <- P.many1 letter <* P.count 2 eol
  rules <- rule `P.endBy1` eol <* P.eof
  return (template, rules)
  where
    rule = do
      before <- letter
      after <- letter
      between <- P.string " -> " *> letter
      return ((before, after), between)
    -- Standard parsers
    letter = P.satisfy C.isLetter
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: (String, [Rule]) -> Integer
solve1 (polymer, ruleL) = score (playNRounds ruleL 10 polymer)

solve2 :: _
solve2 (polymer, ruleL) = score (playNRounds ruleL 40 polymer)

score :: Map a Integer -> Integer
score polymerM = maximum polymerL - minimum polymerL
  where
    polymerL = snd <$> M.toList polymerM

playNRounds :: [Rule] -> Int -> String -> Map Char Integer
playNRounds ruleL n polymer =
  pairs
    & foldl' go (M.empty, M.empty)
    & fst
    & M.unionWith (+) corrections
  where
    rules = M.fromList ruleL
    corrections =
      middles
        & flip zip (repeat (-1))
        & M.fromListWith (+)
    middles =
      pairs
        & drop 1
        & fmap fst
    pairs = zip polymer (drop 1 polymer)
    go (acc, cache) p =
      playN n p cache
        & first (M.unionWith (+) acc)
    playN :: Int -> (Char, Char) -> Cache -> (Map Char Integer, Cache)
    playN n p cache = runMemo (playNMemo n p) cache
    playNMemo ::
      MonadMemo (Int, (Char, Char)) (Map Char Integer) m =>
      Int ->
      (Char, Char) ->
      m (Map Char Integer)
    playNMemo 0 (a, b) = return (M.fromListWith (+) (zip [a, b] (repeat 1)))
    playNMemo n (a, b) =
      case M.lookup (a, b) rules of
        Nothing -> recurse 0 (a, b)
        Just c -> do
          aM <- recurse (n - 1) (a, c)
          bM <- recurse (n - 1) (c, b)
          M.unionWith (+) aM bM
            & M.adjust (\x -> x - 1) c
            & return
      where
        recurse = for2 memo playNMemo

-- The head and drop 1 are necessary for avoiding duplicates. Could use a
-- special kind of concat instead but :shrug:
playRound :: Rules -> String -> String
playRound rules polymer = head polymer : concatMap (drop 1 . maybeInsert) pairs
  where
    maybeInsert p@(a, b) = maybe [a, b] (\x -> [a, x, b]) (M.lookup p rules)
    pairs = zip polymer (drop 1 polymer)

main = do
  input <- readFile "inputs/Day14.txt"
  exampleInput <- readFile "inputs/Day14_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 1588
      solve1 (parse input) @?= 2435
      solve2 (parse exampleInput) @?= 2188189693529
      solve2 (parse input) @?= 2587447599164
