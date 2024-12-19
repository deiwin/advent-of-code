module Day19 (main) where

import Control.Monad.Memo (MonadMemo, memo, startEvalMemo)
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.Maybe (fromJust)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

parse :: String -> ([String], [String])
parse input = run $ do
  patterns <- P.many1 letter `P.sepBy1` P.string ", " <* eol <* eol
  designs <- P.many1 letter `P.endBy1` eol
  return (patterns, designs)
  where
    -- Standard parsers
    letter = P.satisfy C.isLetter
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

possible :: (MonadMemo String Bool m) => [String] -> String -> m Bool
possible _ [] = return True
possible patterns design =
  patterns
    & filter (`L.isPrefixOf` design)
    & traverse (\p -> memo (possible patterns) (drop (length p) design))
    <&> or

solve1 :: ([String], [String]) -> Int
solve1 input =
  designs
    & traverse (possible patterns)
    & startEvalMemo
    & filter id
    & length
  where
    (patterns, designs) = input

possibleN :: (MonadMemo String Int m) => [String] -> String -> m Int
possibleN _ [] = return 1
possibleN patterns design =
  patterns
    & filter (`L.isPrefixOf` design)
    & traverse (\p -> memo (possibleN patterns) (drop (length p) design))
    <&> sum

solve2 :: ([String], [String]) -> Int
solve2 input =
  designs
    & traverse (possibleN patterns)
    & startEvalMemo
    & sum
  where
    (patterns, designs) = input

main = do
  input <- readFile "inputs/Day19.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 317
      solve2 (parse input) @?= 883443544805484
