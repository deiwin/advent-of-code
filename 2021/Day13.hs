module Day13 (main) where

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

data Dim = X | Y
  deriving (Eq, Show, Read)

type Bounds = (V2 Int, V2 Int)

parse :: String -> _
parse input = run $ do
  dots <- dot `P.endBy1` eol <* eol
  folds <- fold `P.endBy1` eol
  P.eof
  return (dots, folds)
  where
    fold :: P.ReadP (Dim, Int)
    fold = do
      P.string "fold along "
      dim <- read . (: []) . C.toUpper <$> letter
      a <- P.char '=' *> number
      return (dim, a)
    -- dim = `
    dot = do
      x <- number <* P.char ','
      y <- number
      return (V2 y x)
    -- Standard parsers
    letter = P.satisfy C.isLetter
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: _
solve1 input = S.size $ fold dotSet (head (snd input))
  where
    dotSet :: Set (V2 Int)
    dotSet = S.fromList (fst input)

boundsFor :: Set (V2 Int) -> Bounds
boundsFor dotSet = (V2 0 0, V2 yMax xMax)
  where
    dotList = S.toList dotSet
    xMax = maximum (getX <$> dotList)
    yMax = maximum (getY <$> dotList)
    getX (V2 _ x) = x
    getY (V2 y _) = y

fold :: Set (V2 Int) -> (Dim, Int) -> Set (V2 Int)
fold dotSet foldInstruction = S.map toNew dotSet
  where
    toNew c@(V2 y x)
      | shouldFoldY = V2 (foldWith maxY y) x
      | shouldFoldX = V2 y (foldWith maxX x)
      | otherwise = c
      where
        foldWith _max real = foldPoint - (real - foldPoint)
        -- foldWith _max real = real
        foldPoint = snd foldInstruction
        shouldFoldY =
          fst foldInstruction == Y
            && y > foldPoint
        shouldFoldX =
          fst foldInstruction == X
            && x > foldPoint
    bounds = boundsFor dotSet
    (_, V2 maxY maxX) = bounds

main = do
  input <- readFile "inputs/Day13.txt"
  exampleInput <- readFile "inputs/Day13_example.txt"
  print $ solve1 $ parse input
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 17
      solve1 (parse input) @?= 704
      1 @?= 1
