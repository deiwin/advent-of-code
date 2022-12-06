module Day05 (main) where

import Control.Applicative ((<|>))
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromJust)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

data Move = Move
  { count :: Int,
    from :: Int,
    to :: Int
  }
  deriving (Show)

type Stacks = Map Int String

parse :: String -> ([String], [Move])
parse input = run $ do
  stacks <- fmap catMaybes . L.transpose <$> stackRow `P.endBy1` eol
  spaces *> number `P.endBy1` spaces <* eol <* eol
  moves <- move `P.endBy1` eol
  return (stacks, moves)
  where
    move = do
      count <- P.string "move " *> number
      from <- P.string " from " *> number
      to <- P.string " to " *> number
      return $ Move {..}
    stackRow = maybeCrate `P.sepBy1` P.char ' '
    maybeCrate =
      Just <$> crate
        <|> P.count 3 (P.char ' ') $> Nothing
    crate = P.char '[' *> letter <* P.char ']'
    -- Standard parsers
    letter = P.satisfy C.isLetter
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    spaces = P.many1 (P.char ' ')
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

makeMove :: Bool -> Stacks -> Move -> Stacks
makeMove shouldReverse s Move {count, from, to} =
  s
    & M.update (Just . drop count) from
    & M.update (Just . (f toMove ++)) to
  where
    toMove = take count (s M.! from)
    f
      | shouldReverse = reverse
      | otherwise = id

solve1 :: ([String], [Move]) -> String
solve1 (stacks, moves) =
  stacks
    & M.fromList . zip [1 ..]
    & flip (L.foldl' (makeMove True)) moves
    & M.toList
    <&> head . snd

solve2 :: ([String], [Move]) -> String
solve2 (stacks, moves) =
  stacks
    & M.fromList . zip [1 ..]
    & flip (L.foldl' (makeMove False)) moves
    & M.toList
    <&> head . snd

main = do
  input <- readFile "inputs/Day05.txt"
  exampleInput <- readFile "inputs/Day05_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= "CMZ"
      solve1 (parse input) @?= "CVCWCRTVQ"
      solve2 (parse exampleInput) @?= "MCD"
      solve2 (parse input) @?= "CNSCZWLVT"
