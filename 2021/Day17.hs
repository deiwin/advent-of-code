module Day17 (main) where

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

parse :: String -> (V2 Int, V2 Int)
parse input = run $ do
  xFrom <- P.string "target area: x=" *> number <* P.string ".."
  xTo <- number <* P.string ", "
  yFrom <- P.string "y=" *> number <* P.string ".."
  yTo <- number <* eol <* P.eof
  return (V2 (min yFrom yTo) (min xFrom xTo), V2 (max yFrom yTo) (max xFrom xTo))
  where
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 (\c -> C.isDigit c || c == '-')
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: _
solve1 targetArea = maxY
  where
    maxY = ((1 + ySpeed) * ySpeed) `div` 2
    ySpeed = abs (getY $ fst targetArea) - 1
    getY (V2 y _) = y
  -- guesses
  --   & fmap (shoot targetArea)
  --   & filter passTargetArea
  --   & fmap highestY
  --   -- & maximum
  -- where
  --   xRange = [(getX (fst targetArea))..(getX (snd targetArea))]
  --   yRange = [(getY (fst targetArea))..(getY (snd targetArea))]
  --   -- xDistForYSpeed vX vY = 
  --   guesses = [V2 y x | y <- [90..147], x <- [0..200]]
  --   -- guesses = [V2 9 6]
  --   passTargetArea :: [V2 Int] -> Bool
  --   passTargetArea = any (inRange targetArea)
  --   highestY :: [V2 Int] -> Int
  --   highestY = maximum . fmap getY
  --   getY (V2 y _) = y
  --   getX (V2 _ x) = x

shoot :: (V2 Int, V2 Int) -> V2 Int -> _
shoot targetArea velocity = relevantPositions
  where
    relevantPositions =
      zip3 allPositions (drop 1 allPositions) (drop 2 allPositions)
        -- & takeWhile (not . pastTheMark)
        -- & takeWhile (\x -> (not . pastTheMark) x || True)
        & take 2
        & unzip3Last
    allPositions = snd <$> iterate (\(v, p) -> (updateVelocity v, p + v)) (velocity, V2 0 0)
    updateVelocity (V2 y x) = V2 (y - 1) (drag x)
    drag 0 = 0
    drag a
      | a < 0 = a + 1
      | otherwise = a - 1
    pastTheMark (a, b, c)
      | traceShow (distanceToTarget <$> [a, b, c]) False = undefined
      | distanceToTarget b <= distanceToTarget a = False
      | distanceToTarget b - distanceToTarget a >= distanceToTarget c - distanceToTarget b = False
      | otherwise = True
    distanceToTarget t@(V2 y x) =
      case targetArea of
        (V2 yA xA, V2 yB xB) -> distance t (V2 (closest y yA yB) (closest x xA xB))
    closest a from to
      | a >= from && a <= to = a
      | a > to = to
      | otherwise = from
    distance a b =
      case a - b of
        V2 y x -> abs y + abs x
    unzip3Last ((a, b, c):xs) = [a, b, c] ++ fmap (\(_, _, c) -> c) xs
    unzip3Last [] = []

main = do
  input <- readFile "inputs/Day17.txt"
  -- exampleInput <- readFile "inputs/Day17_example.txt"
  print $ solve1 $ parse input
  -- print $ solve1 (V2 (-10) 20, V2 (-5) 30)
  runTestTT $
    TestCase $ do
      1 @?= 2
      1 @?= 1
