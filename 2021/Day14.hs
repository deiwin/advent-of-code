{-# LANGUAGE FlexibleContexts #-}

module Day14 (main) where

import Control.Arrow (first)
import Control.Monad.Memo (MonadMemo, for2, memo, runMemo)
import qualified Data.Char as C
import Data.Function ((&))
import Data.List (foldl')
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P

type Rule = ((Char, Char), Char)

type Cache = Map (Int, (Char, Char)) (Map Char Int)

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

solve1 :: (String, [Rule]) -> Int
solve1 (polymer, ruleL) =
  polymer
    & playNRounds ruleL 10
    & score

solve2 :: (String, [Rule]) -> Int
solve2 (polymer, ruleL) =
  polymer
    & playNRounds ruleL 40
    & score

score :: Map a Int -> Int
score polymerM = maximum polymerM - minimum polymerM

playNRounds :: [Rule] -> Int -> String -> Map Char Int
playNRounds ruleL n polymer =
  pairs
    & foldl' go (M.empty, M.empty)
    & fst
    & M.unionWith (+) corrections
  where
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
    ruleM = M.fromList ruleL
    playN :: Int -> (Char, Char) -> Cache -> (Map Char Int, Cache)
    playN n p cache = runMemo (playNMemo n p) cache
    playNMemo ::
      MonadMemo (Int, (Char, Char)) (Map Char Int) m =>
      Int ->
      (Char, Char) ->
      m (Map Char Int)
    playNMemo 0 (a, b) = return (M.fromListWith (+) (zip [a, b] (repeat 1)))
    playNMemo n (a, b) =
      case M.lookup (a, b) ruleM of
        Nothing -> recurse 0 (a, b)
        Just c -> do
          aM <- recurse (n - 1) (a, c)
          bM <- recurse (n - 1) (c, b)
          M.unionWith (+) aM bM
            & M.adjust (\x -> x - 1) c
            & return
      where
        recurse = for2 memo playNMemo

main = do
  input <- readFile "inputs/Day14.txt"
  exampleInput <- readFile "inputs/Day14_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 1588
      solve1 (parse input) @?= 2435
      solve2 (parse exampleInput) @?= 2188189693529
      solve2 (parse input) @?= 2587447599164
