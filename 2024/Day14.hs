module Day14 (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (foldM)
import Data.Char qualified as C
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.Ix (inRange, range)
import Data.List qualified as L
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

type Robot = (V2 Int, V2 Int)

parse :: String -> _
parse input = run $ robot `P.endBy1` eol
  where
    -- Standard parsers
    robot = do
      p <- P.string "p=" *> v2 <* spaces
      v <- P.string "v=" *> v2
      return (p, v)
    v2 = do
      x <- signum <* P.char ','
      y <- signum
      return (V2 y x)
    signum = do
      sign <- P.option 1 ((-1) <$ P.char '-')
      number <- number
      return (sign * number)
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    spaces = P.many1 (P.char ' ')
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

type Bounds = (V2 Int, V2 Int)

smallBounds :: Bounds
smallBounds = (V2 0 0, V2 6 10)

largeBounds :: Bounds
largeBounds = (V2 0 0, V2 102 100)

step :: Bounds -> Robot -> Robot
step bounds (p, v) = (newP, v)
  where
    newP = liftA2 mod (p + v) upperBound
    upperBound = return 1 + snd bounds

stepN :: Bounds -> Int -> Robot -> Robot
stepN bounds n = (!! n) . iterate (step bounds)

y :: V2 Int -> Int
y (V2 y _) = y

x :: V2 Int -> Int
x (V2 _ x) = x

quadrants :: Bounds -> [Bounds]
quadrants (lower, upper) =
  [ (lower, lowMid),
    (V2 (y lower) (x highMid), V2 (y lowMid) (x upper)),
    (V2 (y highMid) (x lower), V2 (y upper) (x lowMid)),
    (highMid, upper)
  ]
  where
    lowMid = (+ (-1)) <$> mid
    highMid = (+ 1) <$> mid
    mid = (`div` 2) <$> lower + upper

solve1 :: Bounds -> [Robot] -> Int
solve1 bounds input =
  quadrants bounds
    <&> (\bounds -> length $ filter (inRange bounds . fst) bots)
    & product
  where
    bots = stepN bounds 100 <$> input

showGrid :: Bounds -> [Robot] -> String
showGrid bounds bots =
  range bounds
    & L.groupBy ((==) `on` y)
    <&> fmap (\p -> if p `S.member` botsS then '#' else '.')
    & unlines
  where
    botsS = S.fromList $ fmap fst bots

solve2 :: Bounds -> [Robot] -> IO (Integer, [Robot])
solve2 bounds input = foldM go (0, input) [0 .. 7800]
  where
    go (i, bots) _ = do
      print i
      putStrLn $ showGrid bounds bots
      threadDelay 10000
      return (i + 1, step bounds <$> bots)

main = do
  input <- readFile "inputs/Day14.txt"
  exampleInput <- readFile "inputs/Day14_example.txt"
  runTestTT $
    TestCase $ do
      solve1 smallBounds (parse exampleInput) @?= 12
      solve1 largeBounds (parse input) @?= 225521010
  -- solve2 largeBounds (parse input) @?= 7774
  solve2 largeBounds $ parse input
