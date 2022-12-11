module Day11 (main) where

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl')
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P
import Prelude hiding (round)

data Val = Val Int | Old
  deriving (Eq, Show)

data Op = Plus Val | Times Val
  deriving (Eq, Show)

data Monkey = Monkey
  { id :: Int,
    items :: [Int],
    operation :: Op,
    divisibleBy :: Int,
    trueMonkey :: Int,
    falseMonkey :: Int,
    inspectionCount :: Int
  }
  deriving (Eq, Show)

parse :: String -> [Monkey]
parse input = run $ monkey `P.sepBy` eol
  where
    monkey = do
      id <- P.string "Monkey " *> number <* P.char ':' <* eol
      items <- P.string "  Starting items: " *> (number `P.sepBy` P.string ", ") <* eol
      operation <- P.string "  Operation: new = old " *> op <* eol
      divisibleBy <- P.string "  Test: divisible by " *> number <* eol
      trueMonkey <- P.string "    If true: throw to monkey " *> number <* eol
      falseMonkey <- P.string "    If false: throw to monkey " *> number <* eol
      let inspectionCount = 0
      return $ Monkey {..}
    op =
      Plus <$> (P.string "+ " *> val)
        <|> Times <$> (P.string "* " *> val)
    val =
      Old <$ P.string "old"
        <|> Val <$> number
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

type State = Map Int Monkey

step :: (Int -> Int) -> State -> Int -> State
step simplify monkeyMap i
  | L.null (monkeyMap M.! i).items = monkeyMap
  | otherwise = newMonkeyMap
  where
    newMonkeyMap =
      monkeyMap
        & M.update (\m -> Just (m {items = drop 1 m.items, inspectionCount = m.inspectionCount + 1})) i
        & M.update (\m -> Just (m {items = m.items ++ [worry]})) monkeyToThrowTo
    monkey = monkeyMap M.! i
    item = head monkey.items
    worry = simplify (op item)
    op =
      case monkey.operation of
        Plus Old -> (* 2)
        Times Old -> (^ 2)
        Plus (Val x) -> (+ x)
        Times (Val x) -> (* x)
    monkeyToThrowTo
      | worry `mod` monkey.divisibleBy == 0 = monkey.trueMonkey
      | otherwise = monkey.falseMonkey

turn :: (Int -> Int) -> State -> Int -> State
turn simplify monkeyMap i
  | L.null (monkeyMap M.! i).items = monkeyMap
  | otherwise = turn simplify (step simplify monkeyMap i) i

round :: (Int -> Int) -> State -> State
round simplify monkeyMap =
  monkeyMap
    & M.toAscList
    <&> fst
    & foldl' (turn simplify) monkeyMap

toState :: [Monkey] -> State
toState =
  fmap (\m -> (m.id, m))
    >>> M.fromList

score :: State -> Int
score =
  M.toList
    >>> fmap (inspectionCount . snd)
    >>> L.sort
    >>> reverse
    >>> take 2
    >>> product

solve1 :: [Monkey] -> Int
solve1 input =
  input
    & toState
    & L.iterate' (round (`div` 3))
    & (L.!! 20)
    & score

solve2 :: [Monkey] -> Int
solve2 input =
  input
    & toState
    & L.iterate' (round (`mod` l))
    & (L.!! 10000)
    & score
  where
    l = L.foldl1' lcm (divisibleBy <$> M.elems (toState input))

main = do
  input <- readFile "inputs/Day11.txt"
  exampleInput <- readFile "inputs/Day11_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 10605
      solve1 (parse input) @?= 316888
      solve2 (parse exampleInput) @?= 2713310158
      solve2 (parse input) @?= 35270398814
