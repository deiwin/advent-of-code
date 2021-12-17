module Day17 (main) where

import qualified Data.Char as C
import Data.Function ((&))
import Data.Ix (inRange)
import qualified Data.List as L
import Data.Maybe (fromJust)
import Linear.V2 (V2 (..))
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

solve1 :: (V2 Int, V2 Int) -> Int
solve1 targetArea = maxY
  where
    maxY = ((1 + ySpeed) * ySpeed) `div` 2
    ySpeed = abs (getY $ fst targetArea) - 1
    getY (V2 y _) = y

solve2 :: (V2 Int, V2 Int) -> Int
solve2 targetArea =
  guesses
    & fmap (shoot targetArea)
    & filter passTargetArea
    & length
  where
    guesses = [V2 y x | y <- yRange, x <- xRange]
    yRange = [((-1) * max (abs yMin) (abs yMax)) .. (max (abs yMin) (abs yMax))]
    xRange = [((-1) * max (abs xMin) (abs xMax)) .. (max (abs xMin) (abs xMax))]
    passTargetArea :: [V2 Int] -> Bool
    passTargetArea = any (inRange targetArea)
    (V2 yMin xMin, V2 yMax xMax) = targetArea

shoot :: (V2 Int, V2 Int) -> V2 Int -> [V2 Int]
shoot targetArea velocity = positions
  where
    positions = L.unfoldr simulate (velocity, V2 0 0)
    simulate (velocity, position)
      | pastTheMark (velocity, position) = Nothing
      | otherwise = Just (position, (updateVelocity velocity, position + velocity))
    updateVelocity (V2 y x) = V2 (y - 1) (drag x)
    drag a = a - signum a
    pastTheMark (V2 yV xV, V2 yP xP)
      | xV == 0 && yV < 0 && movingAway yP yV yMin yMax = True
      | movingAway yP yV yMin yMax && movingAway xP xV xMin xMax = True
      | otherwise = False
    movingAway position velocity min max
      | min <= position && position <= max = False
      | abs (position - min) <= abs (position + velocity - min)
          && abs (position - max) <= abs (position + velocity - max) =
        True
      | otherwise = False
    (V2 yMin xMin, V2 yMax xMax) = targetArea

main = do
  input <- readFile "inputs/Day17.txt"
  runTestTT $
    TestCase $ do
      solve1 (V2 (-10) 20, V2 (-5) 30) @?= 45
      solve1 (parse input) @?= 10585
      solve2 (V2 (-10) 20, V2 (-5) 30) @?= 112
      solve2 (parse input) @?= 5247
