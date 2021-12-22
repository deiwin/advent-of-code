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

parse :: String -> _
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

solve1 :: _
solve1 input =
  input
    & filter (inRange initializationBounds . fst . snd)
    & foldl' merge S.empty
    & S.size
  where
    merge set (True, bounds) = S.union set (S.fromList (range bounds))
    merge set (False, bounds) = set S.\\ S.fromList (range bounds)
    initializationBounds = (V3 (-50) (-50) (-50), V3 50 50 50)

main = do
  input <- readFile "inputs/Day22.txt"
  exampleInput <- readFile "inputs/Day22_example.txt"
  print $ solve1 $ parse exampleInput
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 590784
      solve1 (parse input) @?= 590784
