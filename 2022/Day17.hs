module Day17 (main) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Ix (range)
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Prelude hiding (Either (..))

data Jet = Right | Left
  deriving (Eq, Show)

parse :: String -> [Jet]
parse = fmap toDir . head . lines
  where
    toDir = \case
      '<' -> Left
      '>' -> Right
      _ -> undefined

type Coord = V2 Int

type Shape = [Coord]

shapes :: [Shape]
shapes =
  [ verBar,
    plus,
    reverseL,
    horBar,
    square
  ]
  where
    verBar = V2 0 <$> [0 .. 3]
    plus = V2 0 1 : V2 2 1 : (V2 1 <$> [0 .. 2])
    reverseL = V2 1 2 : V2 2 2 : (V2 0 <$> [0 .. 2])
    horBar = (`V2` 0) <$> [0 .. 3]
    square = [V2 y x | y <- [0, 1], x <- [0, 1]]

data State = State
  { nextShapes :: [Shape],
    nextJets :: [Jet],
    rocks :: Set Coord
  }
  deriving (Eq)

instance Show State where
  show s =
    range bounds
      <&> (\c -> (c, toChar c))
      & L.groupBy (equating (y . fst))
      & reverse
      <&> fmap snd
      & unlines
    where
      toChar = \case
        (V2 (-1) _) -> '-'
        (V2 _ (-1)) -> '|'
        (V2 _ 7) -> '|'
        c
          | c `S.member` s.rocks -> '#'
          | otherwise -> '.'
      bounds = (V2 (-1) (-1), V2 (maxY s.rocks + 3) 7)
      y (V2 y _) = y
      equating f a b = f a == f b

dropShape :: State -> State
dropShape s =
  go (s.rocks, s.nextJets) (create (head s.nextShapes))
    & (\ ~(rocks, nextJets) -> let nextShapes = tail s.nextShapes in State {..})
  where
    create shape = (+ V2 (maxY s.rocks + 4) 2) <$> shape
    go :: (Set Coord, [Jet]) -> Shape -> (Set Coord, [Jet])
    go ~(rocks, jet : jets) shape
      | collides afterDownMove = (rocks `S.union` S.fromList afterJetMove, jets)
      | otherwise = go (rocks, jets) afterDownMove
      where
        collides :: Shape -> Bool
        collides shape = hitsRocks || hitsWall || hitsGround
          where
            hitsRocks = any (`S.member` rocks) shape
            hitsWall = any ((\x -> x < 0 || x > 6) . x) shape
            hitsGround = any ((< 0) . y) shape
            x (V2 _ x) = x
            y (V2 y _) = y
        afterJetMove
          | collides newShape = shape
          | otherwise = newShape
          where
            newShape =
              case jet of
                Left -> (+ V2 0 (-1)) <$> shape
                Right -> (+ V2 0 1) <$> shape
        afterDownMove = (+ V2 (-1) 0) <$> afterJetMove

maxY :: Set Coord -> Int
maxY cs
  | S.null cs = -1
  | otherwise = maximum $ (\(V2 y _) -> y) <$> S.toList cs

solve1 :: [Jet] -> Int
solve1 jets =
  initialState
    & iterate dropShape
    & (L.!! 2022)
    & rocks
    & maxY
    & (+ 1)
  where
    initialState =
      State
        { rocks = S.empty,
          nextShapes = cycle shapes,
          nextJets = cycle jets
        }

solve2 :: [Jet] -> Int
solve2 jets =
  initialState
    & iterate dropShape
    & scoreAt 1000000000000
  where
    scoreAt :: Int -> [State] -> Int
    scoreAt n results
      | n < start = score (results L.!! n)
      | otherwise = atN (start + ((n - start) `mod` cycleLength)) + (cycleSum * ((n - start) `div` cycleLength))
      where
        start = 2000
        atN n = score (results L.!! n)
        cycleLength = length cycle
        cycleSum = sum cycle
        cycle = head $ mapMaybe maybeCycle [20 .. 50000]
        maybeCycle n
          | first == second && first == third = Just first
          | otherwise = Nothing
          where
            diffs = (\xs -> zipWith (-) (drop 1 xs) xs) (score <$> drop start results)
            first = take n diffs
            second = take n $ drop n diffs
            third = take n $ drop (n * 2) diffs
    score s = maxY s.rocks + 1
    initialState =
      State
        { rocks = S.empty,
          nextShapes = cycle shapes,
          nextJets = cycle jets
        }

main = do
  input <- readFile "inputs/Day17.txt"
  exampleInput <- readFile "inputs/Day17_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 3068
      solve1 (parse input) @?= 3117
      solve2 (parse exampleInput) @?= 1514285714288
      solve2 (parse input) @?= 1553314121019
