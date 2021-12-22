module Day22 (main) where

import Control.Applicative (empty, (<|>))
import Control.Arrow (second, (>>>))
import Control.Monad (guard)
import Criterion.Main
  ( bench,
    defaultMain,
    whnf,
  )
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import qualified Data.Char as C
import Data.Function ((&))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Ix
  ( inRange,
    range,
    rangeSize
  )
import Data.List
  ( foldl',
    foldl1',
    isPrefixOf,
    iterate,
  )
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
  ( catMaybes,
    fromJust,
    isJust,
  )
import Data.Ord (comparing)
import Data.Sequence
  ( Seq (..),
    (<|),
    (|>),
  )
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import Data.Void (Void)
import Debug.Trace
  ( traceShow,
    traceShowId,
  )
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P
import Control.Arrow (second)

type Range = (V3 Int, V3 Int)

parse :: String -> [(Bool, Range)]
parse input = run (line `P.endBy1` eol <* P.eof)
  where
    line = do
      switch <- (True <$ P.string "on" <|> False <$ P.string "off") <* spaces
      (xFrom, xTo) <- P.string "x=" *> range <* P.char ','
      (yFrom, yTo) <- P.string "y=" *> range <* P.char ','
      (zFrom, zTo) <- P.string "z=" *> range
      return (switch, (V3 zFrom yFrom xFrom, V3 zTo yTo xTo))
    range = do
      from <- number <* P.string ".."
      to <- number
      return (from, to)
    -- Standard parsers
    number :: P.ReadP Int
    number = do
      sign <- P.option "" ((: []) <$> P.char '-')
      digits <- P.munch1 C.isDigit
      return (read (sign ++ digits))
    spaces = P.many1 (P.char ' ')
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: [(Bool, Range)] -> Int
solve1 input =
  input
    & filter (inRange initializationBounds . fst . snd)
    & foldl' merge S.empty
    & S.size
  where
    merge set (True, bounds) = S.union set (S.fromList (range bounds))
    merge set (False, bounds) = set S.\\ S.fromList (range bounds)
    initializationBounds = (V3 (-50) (-50) (-50), V3 50 50 50)

solve2 :: [(Bool, Range)] -> Integer
solve2 input =
  input
    -- & take 20
    & foldl' go S.empty
    & S.toList
    & fmap (fromIntegral . rangeSize)
    & sum
  where
    go volumes (False, _) | S.null volumes = S.empty
    go volumes (True, volume) | S.null volumes = S.singleton volume
    go volumes new
      | traceShow (length volumes) False = undefined
      | otherwise = S.foldr (\v s -> S.union s (S.fromList (merge v new))) S.empty volumes

merge :: Range -> (Bool, Range) -> [Range]
merge base (False, new)
  | not (overlap base new) = [base]
  | new `subsumes` base = []
merge base (True, new)
  | not (overlap base new) = [base, new]
  | new `subsumes` base = [new]
  | base `subsumes` new = [base]
merge base (keep, new)
  | keep = baseVolumes ++ sharedVolumes ++ newVolumes
  | otherwise = baseVolumes
  where
    (baseVolumes, sharedVolumes, newVolumes) = split base new

split :: Range -> Range -> ([Range], [Range], [Range])
split a b = (aVolumes, sharedVolumes, bVolumes)
  where
    aVolumes =
      allVolumes
        & filter (a `subsumes`)
        & (L.\\ sharedVolumes)
    bVolumes =
      allVolumes
        & filter (b `subsumes`)
        & (L.\\ sharedVolumes)
    sharedVolumes = filter (\v -> a `subsumes` v && b `subsumes` v) allVolumes
    allVolumes = foldl' go [a, b] splitPoints
    go volumes p = concatMap (`splitOnPoint` p) volumes
    splitPoints =
      filter (inRange a) (corners b)
        & (++ filter (inRange b) (corners a))
        & L.nub

splitOnPoint :: Range -> V3 Int -> [Range]
splitOnPoint volume@(V3 z1 y1 x1, V3 z2 y2 x2) p@(V3 zp yp xp)
  | not (inRange volume p) = [volume]
  | otherwise = filter ((> 0) . rangeSize) $ do
    (zFrom, zTo) <- [(z1, zp - 1), (zp, z2)]
    (yFrom, yTo) <- [(y1, yp - 1), (yp, y2)]
    (xFrom, xTo) <- [(x1, xp - 1), (xp, x2)]
    return (V3 zFrom yFrom xFrom, V3 zTo yTo xTo)

subsumes :: Range -> Range -> Bool
subsumes a b = all (inRange a) (corners b)

overlap :: Range -> Range -> Bool
overlap a b = any (inRange a) (corners b) || any (inRange b) (corners a)

corners :: Range -> [V3 Int]
corners (V3 zFrom yFrom xFrom, V3 zTo yTo xTo) =
  [V3 z y x | z <- [zFrom, zTo], y <- [yFrom, yTo], x <- [xFrom, xTo]]

selfCartProd :: Eq a => [a] -> [(a, a)]
selfCartProd xs = [(x, y) | (i, x) <- zip [0 ..] xs, y <- drop (i + 1) xs]

cartProd :: [a] -> [a] -> [(a, a)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

main = do
  input <- readFile "inputs/Day22.txt"
  exampleInput1 <- readFile "inputs/Day22_example1.txt"
  exampleInput2 <- readFile "inputs/Day22_example2.txt"
  print $ solve2 $ parse exampleInput2
  runTestTT $
    TestCase $ do
      1 @?= 1

-- solve1 (parse exampleInput) @?= 590784
-- solve1 (parse input) @?= 590784
