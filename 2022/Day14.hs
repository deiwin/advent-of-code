module Day14 (main) where

import Control.Monad (foldM)
import Data.Char qualified as C
import Data.Either (fromLeft)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

type Coord = V2 Int

data State = State
  { maxY :: Int,
    wallCount :: Int,
    occupied :: Set Coord
  }
  deriving (Show, Eq)

parse :: String -> [[Coord]]
parse input = run $ line `P.endBy` eol
  where
    line = decimal `P.sepBy1` P.string " -> "
    decimal = flip V2 <$> (number <* P.char ',') <*> number
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

originCoord :: Coord
originCoord = V2 0 500

dropSand :: State -> Either State State
dropSand s =
  case newCoord of
    Nothing -> Left s
    Just c -> Right (s {occupied = S.insert c s.occupied})
  where
    newCoord = go originCoord
    go c@(V2 y _x)
      | y >= s.maxY = Nothing
      | otherwise =
          case nextOptions of
            [] -> Just c
            (next : _) -> go next
      where
        nextOptions =
          [V2 1 0, V2 1 (-1), V2 1 1]
            <&> (+ c)
            & filter (not . (`S.member` s.occupied))

linesToState :: [[Coord]] -> State
linesToState xss = State {..}
  where
    wallCount = S.size occupied
    occupied =
      xss
        <&> toSet
        & L.foldl1' S.union
    maxY =
      xss
        & concatMap (fmap (\(V2 y _x) -> y))
        & maximum
    toLine (V2 ay ax) (V2 by bx)
      | ay == by = V2 ay <$> [min ax bx .. max ax bx]
      | otherwise = flip V2 ax <$> [min ay by .. max ay by]
    toSet xs =
      zipWith toLine xs (drop 1 xs)
        & concat
        & S.fromList

runEither :: (a -> Either a a) -> a -> a
runEither f x = fromLeft undefined (foldM (\x _ -> f x) x [0 ..])

solve1 :: [[Coord]] -> Int
solve1 input =
  input
    & linesToState
    & runEither dropSand
    & (\s -> S.size s.occupied - s.wallCount)

dropSand' :: State -> Either State State
dropSand' s
  | newCoord == originCoord = Left newState
  | otherwise = Right newState
  where
    newState = s {occupied = S.insert newCoord s.occupied}
    newCoord = go originCoord
    go c =
      case nextOptions of
        [] -> c
        (next : _) -> go next
      where
        nextOptions =
          [V2 1 0, V2 1 (-1), V2 1 1]
            <&> (+ c)
            & filter (not . (`S.member` s.occupied))
            & filter (\(V2 y _x) -> y < s.maxY + 2)

solve2 :: [[Coord]] -> Int
solve2 input =
  input
    & linesToState
    & runEither dropSand'
    & (\s -> S.size s.occupied - s.wallCount)

main = do
  input <- readFile "inputs/Day14.txt"
  exampleInput <- readFile "inputs/Day14_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 24
      solve1 (parse input) @?= 817
      solve2 (parse exampleInput) @?= 93
      solve2 (parse input) @?= 23416
