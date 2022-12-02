module Day02 (main) where

import Control.Applicative ((<|>))
import Data.Function ((&))
import Data.Functor (($>))
import Data.List qualified as L
import Data.Maybe (fromJust)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

data Hand = Rock | Paper | Scissors
  deriving (Show, Eq, Enum, Bounded)

parse :: String -> [(Hand, Hand)]
parse input = run $ pair `P.endBy1` eol
  where
    pair = do
      a <- opponent <* P.char ' '
      b <- self
      return (a, b)
    self =
      P.char 'X' $> Rock
        <|> P.char 'Y' $> Paper
        <|> P.char 'Z' $> Scissors
    opponent =
      P.char 'A' $> Rock
        <|> P.char 'B' $> Paper
        <|> P.char 'C' $> Scissors
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

handScore :: Hand -> Int
handScore = \case
  Rock -> 1
  Paper -> 2
  Scissors -> 3

succ' :: (Eq a, Enum a, Bounded a) => a -> a
succ' x
  | maxBound == x = minBound
  | otherwise = succ x

outcomeScore :: (Hand, Hand) -> Int
outcomeScore (x, y)
  | succ' y == x = 0 -- loss
  | x == y = 3 -- tie
  | otherwise = 6 -- win

score :: (Hand, Hand) -> Int
score round@(_, hand) = handScore hand + outcomeScore round

solve1 :: [(Hand, Hand)] -> Int
solve1 input =
  input
    & fmap score
    & sum

updateHand :: (Hand, Hand) -> (Hand, Hand)
updateHand = \case
  (x, Rock) -> (x, findForResult x 0)
  (x, Paper) -> (x, x)
  (x, Scissors) -> (x, findForResult x 6)
  where
    findForResult x s = fromJust $ L.find (\y -> outcomeScore (x, y) == s) allHands
    allHands = [minBound .. maxBound]

solve2 :: [(Hand, Hand)] -> Int
solve2 input =
  input
    & fmap (score . updateHand)
    & sum

main = do
  input <- readFile "inputs/Day02.txt"
  exampleInput <- readFile "inputs/Day02_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 15
      solve1 (parse input) @?= 11906
      solve2 (parse exampleInput) @?= 12
      solve2 (parse input) @?= 11186
