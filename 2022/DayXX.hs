module DayXX (main) where

import Control.Applicative (empty, (<|>))
import Control.Arrow (first, second, (>>>))
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
import Data.Functor ((<$), (<&>), ($>))
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
parse input = run $ do
  word <- P.many1 letter <* P.char ':' <* eol
  numbers <- number `P.sepBy1` spaces
  -- eol *> P.eof
  return (word, numbers)
  where
    -- Standard parsers
    letter = P.satisfy C.isLetter
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    spaces = P.many1 (P.char ' ')
    eol = P.char '\n'
    run p = longestMatch $ P.readP_to_S p input
    longestMatch :: [(a, [b])] -> (a, [b])
    longestMatch = L.minimumBy (comparing (length . snd))
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: _
solve1 input =
  input

solve2 :: _
solve2 input =
  input

main = do
  input <- readFile "inputs/DayXX.txt"
  -- exampleInput <- readFile "inputs/DayXX_example.txt"
  print $ solve1 $ parse input
  -- print $ solve2 $ parse input
  runTestTT $
    TestCase $ do
      1 @?= 2
      1 @?= 1
      -- solve1 (parse input) @?= 2
      -- solve2 (parse input) @?= 1
