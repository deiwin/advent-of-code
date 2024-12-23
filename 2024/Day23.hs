module Day23 (main) where

import Control.Arrow (second)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl1')
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [(String, String)]
parse = fmap f . lines
  where
    f s = (takeWhile (/= '-') s, tail $ dropWhile (/= '-') s)

allPairs :: (Eq a) => [a] -> [(a, a)]
allPairs [] = []
allPairs (x : xs) = fmap (x,) xs ++ allPairs xs

solve1 :: [(String, String)] -> Int
solve1 input =
  connected
    & M.filterWithKey (\k _ -> "t" `L.isPrefixOf` k)
    & M.toList
    & concatMap (uncurry f)
    & length
  where
    f :: String -> Set String -> [(String, String, String)]
    f k vs =
      vs
        & S.toList
        & allPairs
        & filter (uncurry connectedTo)
        & filter (\(a, b) -> k == minimum (filter ("t" `L.isPrefixOf`) [a, b, k]))
        & fmap (\(a, b) -> (k, a, b))
    connectedTo :: String -> String -> Bool
    connectedTo a b = b `S.member` (connected M.! a)
    connected :: Map String (Set String)
    connected =
      input
        & concatMap (\(a, b) -> [(a, b), (b, a)])
        <&> second S.singleton
        & M.fromListWith S.union

next' :: Map String (Set String) -> Set String -> Set (Set String)
next' connected s =
  s
    & S.toList
    <&> (connected M.!)
    & foldl1' S.intersection
    & S.map (`S.insert` s)

next :: Map String (Set String) -> Set (Set String) -> Set (Set String)
next connected = S.unions . S.map (next' connected)

solve2 :: [(String, String)] -> String
solve2 input =
  L.unfoldr (fmap (\acc -> (acc, acc)) . go) initial
    & last
    & S.elemAt 0
    & S.toList
    & L.sort
    & L.intercalate ","
  where
    go :: Set (Set String) -> Maybe (Set (Set String))
    go groups
      | S.null nextGroups = Nothing
      | otherwise = Just nextGroups
      where
        nextGroups = next connected groups
    initial :: Set (Set String)
    initial = S.fromList $ S.singleton <$> M.keys connected
    connected :: Map String (Set String)
    connected =
      input
        & concatMap (\(a, b) -> [(a, b), (b, a)])
        <&> second S.singleton
        & M.fromListWith S.union

main = do
  input <- readFile "inputs/Day23.txt"
  exampleInput <- readFile "inputs/Day23_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 7
      solve1 (parse input) @?= 1308
      solve2 (parse exampleInput) @?= "co,de,ka,ta"
      solve2 (parse input) @?= "bu,fq,fz,pn,rr,st,sv,tr,un,uy,zf,zi,zy"
