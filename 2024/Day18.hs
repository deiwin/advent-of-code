module Day18 (main) where

import Control.Monad (guard)
import Control.Parallel.Strategies (parBuffer, rdeepseq, runEval)
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (spLength)
import Data.Ix (inRange, index, range)
import Data.List qualified as L
import Data.Maybe (fromJust, isNothing)
import Data.Set qualified as S
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

parse :: String -> [V2 Int]
parse input = run (v2 `P.endBy1` eol)
  where
    v2 = do
      x <- number <* P.char ','
      y <- number
      return (V2 y x)
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

type Coord = V2 Int

type NLabel = Coord

type ELabel = Int

type G = Gr NLabel ELabel

type Bounds = (Coord, Coord)

toGraph :: Bounds -> [V2 Int] -> G
toGraph bounds walls = mkGraph nodes edges
  where
    nodes =
      range bounds
        & filter (`notElem` wallsS)
        <&> (\coord -> (ix coord, coord))
    edges = do
      coord <- range bounds
      guard $ coord `notElem` wallsS
      dir <- [V2 0 1, V2 1 0, V2 0 (-1), V2 (-1) 0]
      let newCoord = coord + dir
      guard $ inRange bounds newCoord
      guard $ newCoord `notElem` wallsS
      return (ix coord, ix newCoord, 1)
    wallsS = S.fromList walls
    ix = index bounds

smallBounds :: Bounds
smallBounds = (V2 0 0, V2 6 6)

largeBounds :: Bounds
largeBounds = (V2 0 0, V2 70 70)

solve1 :: Bounds -> Int -> [V2 Int] -> Int
solve1 bounds n input =
  input
    & take n
    & toGraph bounds
    & spLength (ix start) (ix end)
    & fromJust
  where
    ix = index bounds
    (start, end) = bounds

solve2 :: Bounds -> [V2 Int] -> V2 Int
solve2 bounds input =
  input
    & L.inits
    & drop 1
    & reverse
    & map (spLength (ix start) (ix end) . toGraph bounds)
    & parBuffer 8 rdeepseq
    & runEval
    & zip (reverse input)
    & takeWhile (isNothing . snd)
    & last
    & fst
  where
    ix = index bounds
    (start, end) = bounds

main = do
  input <- readFile "inputs/Day18.txt"
  exampleInput <- readFile "inputs/Day18_example.txt"
  runTestTT $
    TestCase $ do
      solve1 smallBounds 12 (parse exampleInput) @?= 22
      solve1 largeBounds 1024 (parse input) @?= 298
      solve2 smallBounds (parse exampleInput) @?= V2 1 6
      solve2 largeBounds (parse input) @?= V2 32 52
