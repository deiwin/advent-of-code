module Day19 (main) where

import Control.Monad (guard)
import qualified Data.Char as C
import Data.Function ((&))
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S
import Debug.Trace (traceShow)
import qualified Linear.Matrix as LM
import Linear.V3 (V3 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P

data Report = Report
  { scannerNumber :: Int,
    beaconCoords :: [V3 Int]
  }
  deriving (Eq, Show)

parse :: String -> [Report]
parse input = run (scannerResult `P.sepBy1` eol <* P.eof)
  where
    scannerResult = do
      scannerNumber <- P.string "--- scanner " *> number <* P.string " ---" <* eol
      beaconCoords <- coord `P.endBy1` eol
      return (Report {..})
    coord = do
      [x, y, z] <- number `P.sepBy1` P.char ','
      return (V3 x y z)
    -- Standard parsers
    number :: P.ReadP Int
    number = do
      sign <- P.option "" ((: []) <$> P.char '-')
      digits <- P.munch1 C.isDigit
      return (read (sign ++ digits))
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: [Report] -> Int
solve1 input =
  input
    & align
    & concatMap (beaconCoords . snd)
    & S.fromList
    & S.size

solve2 :: [Report] -> Int
solve2 input =
  input
    & align
    & fmap fst
    & selfCartProd
    & fmap (uncurry manhattan)
    & maximum

manhattan :: V3 Int -> V3 Int -> Int
manhattan a b = abs x + abs y + abs z
  where
    (V3 x y z) = a - b

align :: [Report] -> [(V3 Int, Report)]
align [] = undefined
align (r : rs) = go [(V3 0 0 0, r)] rs
  where
    go a b | traceShow (length a, length b) False = undefined
    go aligned [] = aligned
    go aligned unaligned = go newAligned newUnaligned
      where
        newUnaligned = L.filter ((/= scannerNumber matchingReport) . scannerNumber) unaligned
        newAligned = (diff, matchingReport) : aligned
        (diff, matchingReport) =
          cartProd (snd <$> aligned) unaligned
            & mapMaybe (uncurry triangulate)
            & head

triangulate :: Report -> Report -> Maybe (V3 Int, Report)
triangulate base comparison = listToMaybe $ do
  rotatedComparison <- allOrientations comparison
  let (diff, overlap) = bestDiff base rotatedComparison
  guard (overlap >= 12)
  return (diff, update rotatedComparison diff)
  where
    bestDiff from to =
      cartProd (beaconCoords from) (beaconCoords to)
        & fmap (uncurry (-))
        & cardinality
        & L.maximumBy (comparing snd)
    update report diff = report {beaconCoords = (+ diff) <$> beaconCoords report}

cardinality :: Ord a => [a] -> [(a, Int)]
cardinality xs = M.toList (M.fromListWith (+) (zip xs (repeat 1)))

allOrientations :: Report -> [Report]
allOrientations report = rotateReport <$> allRotM
  where
    rotateReport rot = report {beaconCoords = (rot LM.!*) <$> beaconCoords report}
    allRotM = L.nub $ do
      xRotN1 <- take 4 (iterate (LM.!*! xRot) xRot)
      xRotN2 <- take 4 (iterate (LM.!*! xRot) xRot)
      yRotN <- take 4 (iterate (LM.!*! yRot) yRot)
      return (xRotN1 LM.!*! yRotN LM.!*! xRotN2)
    xRot =
      V3
        (V3 1 0 0)
        (V3 0 0 (-1))
        (V3 0 1 0)
    yRot =
      V3
        (V3 0 0 1)
        (V3 0 1 0)
        (V3 (-1) 0 0)

selfCartProd :: Eq a => [a] -> [(a, a)]
selfCartProd xs = [(x, y) | (i, x) <- zip [0 ..] xs, (j, y) <- zip [0 ..] xs, i /= j]

cartProd :: [a] -> [a] -> [(a, a)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

main = do
  input <- readFile "inputs/Day19.txt"
  exampleInput <- readFile "inputs/Day19_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 79
      solve1 (parse input) @?= 372
      solve2 (parse exampleInput) @?= 3621
      solve2 (parse input) @?= 12241
