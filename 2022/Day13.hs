module Day13 (main) where

import Control.Applicative ((<|>))
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.Maybe (catMaybes, fromJust, listToMaybe)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

data Tree = Tree [Tree] | Leaf Int
  deriving (Eq, Show)

type Pair = (Tree, Tree)

parse :: String -> [Pair]
parse input = run $ pair `P.sepBy` P.count 2 eol <* eol
  where
    pair = do
      first <- tree <* eol
      second <- tree
      return (first, second)
    tree = Tree <$> P.between (P.char '[') (P.char ']') (elem `P.sepBy` P.char ',')
    elem = tree <|> Leaf <$> number
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

inOrder :: Pair -> Bool
inOrder = fromJust . go
  where
    go = \case
      (Leaf l, Leaf r)
        | l == r -> Nothing
        | otherwise -> Just (l < r)
      (Leaf l, Tree r) -> go (Tree [Leaf l], Tree r)
      (Tree l, Leaf r) -> go (Tree l, Tree [Leaf r])
      (Tree l, Tree r) ->
        zip l r
          <&> go
          & (++ [lastElem])
          & catMaybes
          & listToMaybe
        where
          lastElem
            | length l == length r = Nothing
            | length l < length r = Just True
            | otherwise = Just False

solve1 :: [Pair] -> Int
solve1 input =
  input
    <&> inOrder
    & zip [1 ..]
    & filter snd
    <&> fst
    & sum

instance Ord Tree where
  a <= b = inOrder (a, b)

solve2 :: [Pair] -> Int
solve2 input =
  input
    & concatMap (\(a, b) -> [a, b])
    & (++ dividers)
    & L.sort
    & zip [1 ..]
    & filter ((`elem` dividers) . snd)
    <&> fst
    & product
  where
    dividers = [Tree [Tree [Leaf 2]], Tree [Tree [Leaf 6]]]

main = do
  input <- readFile "inputs/Day13.txt"
  exampleInput <- readFile "inputs/Day13_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 13
      solve1 (parse input) @?= 6623
      solve2 (parse exampleInput) @?= 140
      solve2 (parse input) @?= 23049
