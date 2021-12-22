module Day22 (main) where

import Control.Applicative ((<|>))
import qualified Data.Char as C
import Data.Function ((&))
import Data.Ix ( inRange, range, rangeSize)
import Data.List ( foldl')
import qualified Data.List as L
import Data.Maybe ( fromJust)
import qualified Data.Set as S
import Linear.V3 (V3 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P

type Volume = (V3 Int, V3 Int)

parse :: String -> [(Bool, Volume)]
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

solve1 :: [(Bool, Volume)] -> Int
solve1 input =
  input
    & filter (inRange initializationBounds . fst . snd)
    & foldl' merge S.empty
    & S.size
  where
    merge set (True, v) = S.union set (S.fromList (range v))
    merge set (False, v) = set S.\\ S.fromList (range v)
    initializationBounds = (V3 (-50) (-50) (-50), V3 50 50 50)

solve2 :: [(Bool, Volume)] -> Integer
solve2 input = volumeListSum (withCorrections input)

withCorrections :: [(Bool, Volume)] -> [(Bool, Volume)]
withCorrections = foldl' go []
  where
    go vs v@(False, _) = concatMap (`correction` v) vs
    go vs v@(True, _) = concatMap (`correction` v) vs ++ [v]

volumeListSum :: [(Bool, Volume)] -> Integer
volumeListSum = sum . fmap (fromIntegral . correctionSize)

correctionSize :: (Bool, Volume) -> Int
correctionSize = \case
  (True, v) -> rangeSize v
  (False, v) -> negate (rangeSize v)

correction :: (Bool, Volume) -> (Bool, Volume) -> [(Bool, Volume)]
correction a@(aOn, aVol) (_, bVol) =
  case intersection aVol bVol of
    Nothing -> [a]
    Just interVol ->
      if aVol == interVol
        then []
        else [a, (not aOn, interVol)]

intersection :: Volume -> Volume -> Maybe Volume
intersection a b
  | rangeSize volume > 0 = Just volume
  | otherwise = Nothing
  where
    volume = (V3 z1 y1 x1, V3 z2 y2 x2)
    z1 = max (getZ (fst a)) (getZ (fst b))
    y1 = max (getY (fst a)) (getY (fst b))
    x1 = max (getX (fst a)) (getX (fst b))
    z2 = min (getZ (snd a)) (getZ (snd b))
    y2 = min (getY (snd a)) (getY (snd b))
    x2 = min (getX (snd a)) (getX (snd b))
    getZ (V3 z _ _) = z
    getY (V3 _ y _) = y
    getX (V3 _ _ x) = x

main = do
  input <- readFile "inputs/Day22.txt"
  exampleInput1 <- readFile "inputs/Day22_example1.txt"
  exampleInput2 <- readFile "inputs/Day22_example2.txt"
  runTestTT $
    TestCase $ do
      -- Intersections
      intersection (V3 0 0 0, V3 0 0 0) (V3 0 0 0, V3 0 0 0) @?= Just (V3 0 0 0, V3 0 0 0)
      intersection (V3 0 0 0, V3 1 1 1) (V3 0 0 0, V3 1 1 1) @?= Just (V3 0 0 0, V3 1 1 1)
      intersection (V3 0 0 0, V3 0 0 0) (V3 1 1 1, V3 1 1 1) @?= Nothing
      intersection (V3 0 0 0, V3 0 2 2) (V3 0 1 1, V3 0 2 2) @?= Just (V3 0 1 1, V3 0 2 2)
      intersection (V3 0 0 0, V3 0 2 2) (V3 0 1 1, V3 0 3 3) @?= Just (V3 0 1 1, V3 0 2 2)
      intersection (V3 0 0 0, V3 0 2 2) (V3 0 1 1, V3 0 3 3) @?= Just (V3 0 1 1, V3 0 2 2)
      intersection (V3 0 0 1, V3 0 2 2) (V3 0 1 0, V3 0 3 3) @?= Just (V3 0 1 1, V3 0 2 2)
      -- Corrections
      -- No overlap
      correction (True, (V3 0 0 0, V3 0 0 0)) (True, (V3 1 1 1, V3 1 1 1))
        @?= [(True, (V3 0 0 0, V3 0 0 0))]
      correction (True, (V3 0 0 0, V3 0 0 0)) (False, (V3 1 1 1, V3 1 1 1))
        @?= [(True, (V3 0 0 0, V3 0 0 0))]
      -- Exact overlap for True
      correction (True, (V3 0 0 0, V3 0 0 0)) (True, (V3 0 0 0, V3 0 0 0)) @?= []
      correction (True, (V3 0 0 0, V3 0 0 0)) (False, (V3 0 0 0, V3 0 0 0)) @?= []
      -- Partial overlap for True
      correction (True, (V3 0 0 0, V3 0 0 0)) (True, (V3 0 0 0, V3 1 1 1)) @?= []
      correction (True, (V3 0 0 0, V3 0 0 0)) (False, (V3 0 0 0, V3 1 1 1)) @?= []
      -- Exact overlap for False
      correction (False, (V3 0 0 0, V3 0 0 0)) (True, (V3 0 0 0, V3 0 0 0)) @?= []
      correction (False, (V3 0 0 0, V3 0 0 0)) (False, (V3 0 0 0, V3 0 0 0)) @?= []
      -- Sums
      solve2 [(True, (V3 0 0 0, V3 0 0 0))] @?= 1
      solve2 [(True, (V3 0 0 0, V3 1 1 1))] @?= 8
      solve2 [(True, (V3 0 0 0, V3 0 0 0)), (True, (V3 1 1 1, V3 1 1 1))] @?= 2
      solve2 [(True, (V3 0 0 0, V3 0 0 0)), (False, (V3 1 1 1, V3 1 1 1))] @?= 1
      solve2 [(True, (V3 0 0 0, V3 0 0 0)), (False, (V3 0 0 0, V3 0 0 0))] @?= 0
      solve2
        [ (True, (V3 0 0 0, V3 0 0 0)),
          (False, (V3 0 0 0, V3 0 0 0)),
          (False, (V3 0 0 0, V3 0 0 0))
        ]
        @?= 0
      solve2
        [ (True, (V3 0 0 0, V3 0 0 0)),
          (False, (V3 0 0 0, V3 0 0 0)),
          (True, (V3 0 0 0, V3 0 0 0))
        ]
        @?= 1
      solve2
        [ (True, (V3 0 0 0, V3 0 0 0)),
          (False, (V3 0 0 0, V3 0 0 0)),
          (True, (V3 0 0 0, V3 0 0 0)),
          (False, (V3 0 0 0, V3 0 0 0))
        ]
        @?= 0
      solve2
        [ (True, (V3 0 0 0, V3 0 0 0)),
          (False, (V3 0 0 0, V3 0 0 0)),
          (True, (V3 0 0 0, V3 0 0 0)),
          (True, (V3 0 0 0, V3 0 0 0))
        ]
        @?= 1
      solve2
        [ (True, (V3 0 0 0, V3 0 0 0)),
          (False, (V3 0 0 0, V3 0 0 0)),
          (False, (V3 0 0 0, V3 0 0 0)),
          (True, (V3 0 0 0, V3 0 0 0))
        ]
        @?= 1
      solve2
        [ (True, (V3 0 0 0, V3 0 0 0)),
          (False, (V3 0 0 0, V3 1 1 1)),
          (True, (V3 0 0 0, V3 1 1 1))
        ]
        @?= 8
      solve2
        [ (True, (V3 0 0 0, V3 0 0 0)),
          (False, (V3 0 0 0, V3 1 1 1)),
          (True, (V3 1 1 1, V3 1 1 1))
        ]
        @?= 1
      solve2
        [ (True, (V3 1 1 1, V3 1 1 1)),
          (False, (V3 1 1 1, V3 1 1 1)),
          (True, (V3 0 0 0, V3 2 2 2))
        ]
        @?= 27
      solve2
        [ (True, (V3 1 1 1, V3 1 1 1)),
          (True, (V3 0 0 0, V3 2 2 2)),
          (False, (V3 1 1 1, V3 1 1 1))
        ]
        @?= 26
      -- Problematic example
      correction (True, (V3 0 0 0, V3 0 4 3)) (True, (V3 0 2 1, V3 0 3 4))
        @?= [(True, (V3 0 0 0, V3 0 4 3)), (False, (V3 0 2 1, V3 0 3 3))]
      correction (False, (V3 0 1 2, V3 0 4 3)) (True, (V3 0 2 1, V3 0 3 4))
        @?= [(False, (V3 0 1 2, V3 0 4 3)), (True, (V3 0 2 2, V3 0 3 3))]
      correction (True, (V3 0 1 2, V3 0 5 5)) (True, (V3 0 2 1, V3 0 3 4))
        @?= [(True, (V3 0 1 2, V3 0 5 5)), (False, (V3 0 2 2, V3 0 3 4))]
      let problematicInput =
            [ (True, (V3 0 0 0, V3 0 4 3)),
              (True, (V3 0 1 2, V3 0 5 5)),
              (True, (V3 0 2 1, V3 0 3 4))
            ]
      withCorrections problematicInput
        @?= [ (True, (V3 0 0 0, V3 0 4 3)),
              (False, (V3 0 2 1, V3 0 3 3)),
              (False, (V3 0 1 2, V3 0 4 3)),
              (True, (V3 0 2 2, V3 0 3 3)),
              (True, (V3 0 1 2, V3 0 5 5)),
              (False, (V3 0 2 2, V3 0 3 4)),
              (True, (V3 0 2 1, V3 0 3 4))
            ]
      (correctionSize <$> withCorrections problematicInput)
        @?= [20, -6, -8, 4, 20, -6, 8]
      volumeListSum (withCorrections problematicInput)
        @?= 32
      solve2 problematicInput
        @?= 32

      solve1 (parse exampleInput1) @?= 590784
      solve1 (parse input) @?= 564654
      solve2 (parse exampleInput2) @?= 2758514936282235
      solve2 (parse input) @?= 1214193181891104
