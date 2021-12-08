module Day08 (main) where

import Control.Arrow (first, second, (>>>))
import qualified Data.Char as C
import Data.Function ((&))
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P

type Entry = ([String], [String])

parse :: String -> [Entry]
parse input = run $ entry `P.endBy1` eol <* P.eof
  where
    entry = do
      signal <- pattern `P.sepBy1` spaces <* P.string " | "
      output <- pattern `P.sepBy1` spaces
      return (signal, output)
    pattern = P.many1 letter
    -- Standard parsers
    letter = P.satisfy C.isLetter
    spaces = P.many1 (P.char ' ')
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: [Entry] -> Int
solve1 input =
  input
    & concatMap snd
    & filter ((`elem` uniqueCardinalities) . length)
    & length
  where
    uniqueCardinalities =
      numberCardinalities
        & filter ((`elem` [1, 4, 7, 8]) . fst)
        & fmap snd

solve2 :: [Entry] -> Int
solve2 input =
  input
    & fmap (toInt . deduceNumbers)
    & sum

toInt :: [Int] -> Int
toInt =
  reverse
    >>> zip [0 ..]
    >>> fmap (uncurry (*) . first (10 ^))
    >>> sum

deduceNumbers :: Entry -> [Int]
deduceNumbers (signal, output) = (map M.!) . S.fromList <$> output
  where
    map =
      M.fromList
        [ (S.fromList c0, 0),
          (S.fromList c1, 1),
          (S.fromList c2, 2),
          (S.fromList c3, 3),
          (S.fromList c4, 4),
          (S.fromList c5, 5),
          (S.fromList c6, 6),
          (S.fromList c7, 7),
          (S.fromList c8, 8),
          (S.fromList c9, 9)
        ]
    c1 = uniqueCardinalityNr 1
    c4 = uniqueCardinalityNr 4
    c7 = uniqueCardinalityNr 7
    c8 = uniqueCardinalityNr 8
    c235 = cardinalityNr 2
    [c3] = filter (includes c1) c235
    c25 = c235 L.\\ [c3]
    [c5] = filter (L.null . (L.\\ c6)) c25
    [c2] = c25 L.\\ [c5]
    c069 = cardinalityNr 0
    [c6] = filter (not . includes c1) c069
    [c9] = filter (includes c4) c069
    [c0] = c069 L.\\ [c6, c9]
    uniqueCardinalityNr x =
      signal
        & L.find ((== length (canonicalNames M.! x)) . length)
        & fromJust
    cardinalityNr x = filter ((== length (canonicalNames M.! x)) . length) signal
    includes xs inYs = (xs `L.intersect` inYs) == xs

canonicalNames :: Map Int String
canonicalNames =
  M.fromList
    [ (0, "abcefg"),
      (1, "cf"),
      (2, "acdeg"),
      (3, "acdfg"),
      (4, "bcdf"),
      (5, "abdfg"),
      (6, "abdefg"),
      (7, "acf"),
      (8, "abcdefg"),
      (9, "abcdfg")
    ]

numberCardinalities :: [(Int, Int)]
numberCardinalities = second length <$> M.toList canonicalNames

main = do
  input <- readFile "inputs/Day08.txt"
  exampleInput <- readFile "inputs/Day08_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 349
      solve2 (parse exampleInput) @?= 61229
      solve2 (parse input) @?= 1070957
