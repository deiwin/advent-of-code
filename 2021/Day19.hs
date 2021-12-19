module Day19 (main) where

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
    listToMaybe,
    mapMaybe,
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
import qualified Linear.Algebra as LA
import qualified Linear.Matrix as LM
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import qualified Linear.Vector as LV
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P

data Axis = X | Y | Z
  deriving (Eq, Show, Enum)

data Report = Report
  { scannerNumber :: Int,
    beaconCoords :: [V3 Int]
  }
  deriving (Eq, Show)

type Rotation = LM.M33 Int

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

-- solve1 :: _
-- solve1 input =
--   input
--     & align
--     & concatMap (beaconCoords . snd)
--     & S.fromList
--     & S.size

-- solve2 :: _
-- solve2 input =
--   input
--     & align
--     & fmap fst
--     & selfCartProd
--     & fmap (uncurry manhattan)
--     & maximum

-- manhattan :: V3 Int -> V3 Int -> Int
-- manhattan a b = abs x + abs y + abs z
--   where
--     (V3 x y z) = a - b

align :: [Report] -> _ -- [(V3 Int, Report)]
align reports =
  reports
    & selfUniquePairs
    & mapMaybe (\(a, b) -> (a,) <$> triangulate a b)
    & go IM.empty
  where
    go ::
      IntMap (V3 Int, Rotation, Report) ->
      [(Report, (V3 Int, Rotation, Report))] ->
      IntMap (V3 Int, Rotation, Report)
    go a b | traceShow ("sizes", IM.size a, length b) False = undefined
    go alignedMap [] = alignedMap
    go alignedMap (match : rest) | IM.null alignedMap = go initialMap rest
      where
        (base, (diff, rot, comparison)) = match
        initialMap =
          IM.fromList
            [ (scannerNumber base, (V3 0 0 0, idRot, base)),
              (scannerNumber comparison, (diff, rot, comparison))
            ]
    go alignedMap matches
      | traceShow ("found", IM.keys alignedMap) False = undefined
      | traceShow ("rem", showMatch <$> remainingMatches) False = undefined
      | otherwise = go newMap remainingMatches
      where
        showMatch (a, (_, _, b)) = (scannerNumber a, scannerNumber b)
        newMap :: IntMap (V3 Int, Rotation, Report)
        newMap =
          readyMatches
            & fmap (toAssoc . update . findBase)
            & IM.fromList
            & IM.union alignedMap
        findBase (base, match) = (alignedMap IM.! scannerNumber base, match)
        update ((baseDiff, baseRot, _), (diff, rot, comparison)) = (newDiff, newRot, newReport)
          where
            newDiff = baseDiff + (baseRot LM.!* diff)
            newRot = baseRot LM.!*! rot
            newReport = comparison { beaconCoords = updateCoord <$> beaconCoords comparison }
            updateCoord c = baseDiff + (baseRot LM.!* c)
        toAssoc x@(_, _, report) = (scannerNumber report, x)
        (readyMatches@(_:_), remainingMatches) = L.partition ((`IM.member` alignedMap) . scannerNumber . fst) matches

triangulate :: Report -> Report -> Maybe (V3 Int, Rotation, Report)
triangulate base comparison = listToMaybe $ do
  (rot, rotatedComparison) <- allOrientations comparison
  baseFocus <- beaconCoords base
  comparisonFocus <- beaconCoords rotatedComparison
  let baseMod = S.fromList (flip (-) baseFocus <$> beaconCoords base)
  let comparisonMod = S.fromList (flip (-) comparisonFocus <$> beaconCoords rotatedComparison)
  let overlap = S.size (S.intersection baseMod comparisonMod)
  guard (overlap >= 12)
  let diff = baseFocus - comparisonFocus
  let update = fmap (+ diff)
  let updatedComparison = rotatedComparison {beaconCoords = update (beaconCoords rotatedComparison)}
  return (diff, rot, updatedComparison)

allOrientations :: Report -> [(Rotation, Report)]
allOrientations report = rotateReport <$> allRotM
  where
    rotateReport rot = (rot, report {beaconCoords = (rot LM.!*) <$> beaconCoords report})
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

idRot =
  V3
    (V3 1 0 0)
    (V3 0 1 0)
    (V3 0 0 1)

selfUniquePairs :: [a] -> [(a, a)]
selfUniquePairs xs =
  L.tails xs
    & drop 1
    & zip xs
    & concatMap (\(a, bs) -> (a,) <$> bs)

selfCartProd :: Eq a => [a] -> [(a, a)]
selfCartProd xs = [(x, y) | (i, x) <- zip [0 ..] xs, (j, y) <- zip [0 ..] xs, i /= j]

cartProd :: [a] -> [a] -> [(a, a)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

main = do
  input <- readFile "inputs/Day19.txt"
  exampleInput <- readFile "inputs/Day19_example.txt"
  -- print $ solve2 $ parse exampleInput
  print $ length $ align $ parse input
  runTestTT $
    TestCase $ do
      1 @?= 1
      selfUniquePairs [0, 1, 2] @?= [(0, 1), (0, 2), (1, 2)]
      selfUniquePairs [0, 1, 2, 3] @?= [(0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)]

-- solve1 (parse exampleInput) @?= 79
-- solve1 (parse input) @?= 372
-- solve2 (parse exampleInput) @?= 3621

-- solve2 (parse input) @?= 12241
