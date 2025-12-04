module Day04 (main) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Ix (range)
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

type Coord = V2 Int

parse :: String -> HashSet Coord
parse input =
  zip (range bounds) (concat ls)
    & filter ((== '@') . snd)
    <&> fst
    & HS.fromList
  where
    ls = lines input
    bounds = (V2 0 0, V2 (length ls - 1) (length (head ls) - 1))

adjacent :: [Coord]
adjacent =
  [ V2 (-1) (-1),
    V2 (-1) 0,
    V2 (-1) 1,
    V2 0 (-1),
    V2 0 1,
    V2 1 (-1),
    V2 1 0,
    V2 1 1
  ]

accessible :: HashSet Coord -> [Coord]
accessible s =
  HS.toList s
    & filter ((< 4) . adjacentCount)
  where
    adjacentCount c = length $ filter (`HS.member` s) ((c +) <$> adjacent)

solve1 :: HashSet Coord -> Int
solve1 = length . accessible

turn :: HashSet Coord -> HashSet Coord
turn s = foldl' (flip HS.delete) s $ accessible s

untilStable :: (Eq a) => (a -> a) -> a -> a
untilStable f x = snd $ last $ takeWhile (uncurry (/=)) $ zip res (tail res)
  where
    res = iterate f x

solve2 :: HashSet Coord -> Int
solve2 input = HS.size input - HS.size (untilStable turn input)

main = do
  input <- readFile "inputs/Day04.txt"
  exampleInput <- readFile "inputs/Day04_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 13
      solve1 (parse input) @?= 1508
      solve2 (parse exampleInput) @?= 43
      solve2 (parse input) @?= 8538
