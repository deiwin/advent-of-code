module Day18 (main) where

import Control.Applicative (empty, (<|>))
import Control.Arrow (second, (>>>))
import Control.Monad (foldM, guard)
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
    maybe,
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
import Text.Printf (printf)

data Tree a = Leaf Int | Pair (Tree a) (Tree a)
  deriving (Eq)

instance Show (Tree a) where
  show (Leaf x) = show x
  show (Pair left right) = printf "[%s,%s]" (show left) (show right)

parse :: String -> [Tree Int]
parse input = run (tree `P.endBy1` eol <* P.eof)
  where
    tree = P.between (P.char '[') (P.char ']') pair
    pair = do
      left <- leafOrTree <* P.char ','
      Pair left <$> leafOrTree
    leafOrTree =
      Leaf <$> number
        <|> tree
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: [Tree Int] -> _
solve1 input =
  input
    & L.foldl1' addTree
    & magnitude

magnitude :: Tree Int -> Int
magnitude (Leaf x) = x
magnitude (Pair left right) = (3 * magnitude left) + (2 * magnitude right)

addTree :: Tree Int -> Tree Int -> Tree Int
addTree left right = reduce (Pair left right)

reduce :: Tree Int -> Tree Int
reduce tree =
  foldM go tree [0 ..]
    & either id undefined
  where
    go :: Tree Int -> Int -> Either (Tree Int) (Tree Int)
    go tree _ =
      case maybeExplodeOne tree of
        Just newTree -> Right newTree
        Nothing ->
          case maybeSplitOne tree of
            Just newTree -> Right newTree
            Nothing -> Left tree
    maybeExplodeOne :: Tree Int -> Maybe (Tree Int)
    maybeExplodeOne tree = (\(x, _, _) -> x) <$> maybeExplodeOne' 0 tree
    maybeExplodeOne' :: Int -> Tree Int -> Maybe (Tree Int, Maybe Int, Maybe Int)
    maybeExplodeOne' nesting (Pair (Leaf left) (Leaf right))
      | nesting >= 4 = Just (Leaf 0, Just left, Just right)
      | otherwise = Nothing
    maybeExplodeOne' _ (Leaf x) = Nothing
    maybeExplodeOne' nesting (Pair left right) =
      case maybeExplodeOne' (nesting + 1) left of
        Just (newLeft, maybeLeftUpdate, Just rightUpdate) ->
          Just (Pair newLeft (addToLeftmost right rightUpdate), maybeLeftUpdate, Nothing)
        Just (newLeft, maybeLeftUpdate, Nothing) -> Just (Pair newLeft right, maybeLeftUpdate, Nothing)
        Nothing ->
          case maybeExplodeOne' (nesting + 1) right of
            Just (newRight, Just leftUpdate, maybeRightUpdate) ->
              Just (Pair (addToRightmost left leftUpdate) newRight, Nothing, maybeRightUpdate)
            Just (newRight, Nothing, maybeRightUpdate) ->
              Just (Pair left newRight, Nothing, maybeRightUpdate)
            Nothing -> Nothing
    maybeSplitOne :: Tree Int -> Maybe (Tree Int)
    maybeSplitOne (Leaf x)
      | x >= 10 = Just (Pair (Leaf half) (Leaf (half + rem)))
      | otherwise = Nothing
      where
        (half, rem) = x `divMod` 2
    maybeSplitOne (Pair left right) =
      case maybeSplitOne left of
        Just newLeft -> Just (Pair newLeft right)
        Nothing ->
          case maybeSplitOne right of
            Just newRight -> Just (Pair left newRight)
            Nothing -> Nothing
    addToLeftmost :: Tree Int -> Int -> Tree Int
    addToLeftmost (Leaf x) d = Leaf (x + d)
    addToLeftmost (Pair left right) d = Pair (addToLeftmost left d) right
    addToRightmost :: Tree Int -> Int -> Tree Int
    addToRightmost (Leaf x) d = Leaf (x + d)
    addToRightmost (Pair left right) d = Pair left (addToRightmost right d)

main = do
  input <- readFile "inputs/Day18.txt"
  exampleInput1 <- readFile "inputs/Day18_example1.txt"
  exampleInput2 <- readFile "inputs/Day18_example2.txt"
  print $ solve1 $ parse exampleInput1
  runTestTT $
    TestCase $ do
      show (L.foldl1' addTree (parse "[1,1]\n[2,2]\n[3,3]\n[4,4]\n"))
        @?= "[[[[1,1],[2,2]],[3,3]],[4,4]]"
      show (L.foldl1' addTree (parse "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n"))
        @?= "[[[[3,0],[5,3]],[4,4]],[5,5]]"
      show (L.foldl1' addTree (parse "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n[6,6]\n"))
        @?= "[[[[5,0],[7,4]],[5,5]],[6,6]]"
      show (L.foldl1' addTree (parse exampleInput1))
        @?= "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
      solve1 (parse exampleInput1) @?= 3488
      solve1 (parse exampleInput2) @?= 4140
      solve1 (parse input) @?= 4132
