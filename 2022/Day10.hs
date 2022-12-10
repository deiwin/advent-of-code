module Day10 (main) where

import Control.Applicative ((<|>))
import Control.Arrow (first, (>>>))
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

data Command = Addx Int | Noop
  deriving (Show, Eq)

parse :: String -> [Command]
parse input = run $ command `P.endBy` eol
  where
    command =
      Noop <$ P.string "noop"
        <|> Addx <$> (P.string "addx " *> signum)
    signum = do
      sign <- P.option 1 ((-1) <$ P.char '-')
      number <- number
      return (sign * number)
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

process :: [Command] -> [(Int, Int)]
process =
  concatMap addNoop
    >>> L.scanl' f (1, 1)
  where
    addNoop = \case
      Noop -> [Noop]
      Addx x -> [Noop, Addx x]
    f (i, x) = \case
      Noop -> (i + 1, x)
      Addx dx -> (i + 1, x + dx)

solve1 :: [Command] -> Int
solve1 input =
  input
    & process
    & M.fromList
    & result
  where
    result m =
      [20, 60, 100, 140, 180, 220]
        & fmap (\i -> i * m M.! i)
        & sum

solve2 :: [Command] -> String
solve2 input =
  input
    & process
    <&> toPixel . first toCoord
    & L.groupBy (equating (y . fst))
    <&> fmap ((\b -> if b then '#' else '.') . snd)
    & unlines
  where
    y (V2 yc _) = yc
    equating f a b = f a == f b
    toPixel (c@(V2 _ xc), x) = (c, abs (x - xc) <= 1)
    toCoord i = V2 ((i - 1) `div` 40) ((i - 1) `mod` 40)

main = do
  input <- readFile "inputs/Day10.txt"
  exampleInput <- readFile "inputs/Day10_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 13140
      solve1 (parse input) @?= 14040
      solve2 (parse exampleInput)
        @?= "##..##..##..##..##..##..##..##..##..##..\n\
            \###...###...###...###...###...###...###.\n\
            \####....####....####....####....####....\n\
            \#####.....#####.....#####.....#####.....\n\
            \######......######......######......####\n\
            \#######.......#######.......#######.....\n\
            \.\n"
      solve2 (parse input)
        @?= "####..##...##....##.####...##.####.#....\n\
            \...#.#..#.#..#....#....#....#.#....#....\n\
            \..#..#....#.......#...#.....#.###..#....\n\
            \.#...#.##.#.......#..#......#.#....#....\n\
            \#....#..#.#..#.#..#.#....#..#.#....#....\n\
            \####..###..##...##..####..##..#....####.\n\
            \.\n"
