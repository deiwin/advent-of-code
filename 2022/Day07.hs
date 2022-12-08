module Day07 (main) where

import Control.Applicative ((<|>))
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl')
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

data Output
  = ODir {name :: String}
  | OFile
      { name :: String,
        size :: Int
      }
  deriving (Show)

data LogEntry = CD String | LS [Output]
  deriving (Show)

parse :: String -> [LogEntry]
parse input = run $ logEntry `P.endBy1` eol
  where
    logEntry = P.string "$ " *> (cd <|> ls)
    cd = CD <$> (P.string "cd " *> word)
    ls = LS <$> (P.string "ls" *> eol *> output `P.sepBy` eol)
    output = odir <|> ofile
    odir = ODir <$> (P.string "dir " *> word)
    ofile = do
      size <- number <* P.char ' '
      name <- word
      return $ OFile {..}
    word = P.many1 (P.satisfy (not . C.isSpace))
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

filesWithPaths :: [LogEntry] -> Set ([String], Int)
filesWithPaths = snd . foldl' f ([], S.empty) . drop 1
  where
    f (path, files) = \case
      CD ".." -> (drop 1 path, files)
      CD dir -> (dir : path, files)
      LS outputs -> (path, S.union files $ S.fromList $ mapMaybe (g path) outputs)
    g path = \case
      ODir {} -> Nothing
      OFile {name, size} -> Just (name : path, size)

folderSizes :: Set ([String], Int) -> Map [String] Int
folderSizes = S.foldl' f M.empty
  where
    f m (path, size) =
      path
        & drop 1 -- ignore the file itself
        & L.tails
        & foldl' (g size) m
    g size m path = M.insertWith (+) path size m

solve1 :: [LogEntry] -> Int
solve1 input =
  input
    & filesWithPaths
    & folderSizes
    & M.filter (<= 100000)
    & M.foldl' (+) 0

solve2 :: [LogEntry] -> Maybe Int
solve2 input =
  input
    & filesWithPaths
    & folderSizes
    & smallestToDelete
  where
    smallestToDelete m =
      m
        & M.toList
        <&> snd
        & L.sort
        & L.find (>= toDelete)
      where
        toDelete = 30000000 - freeSpace
        freeSpace = 70000000 - usedSpace
        usedSpace = m M.! []

main = do
  input <- readFile "inputs/Day07.txt"
  exampleInput <- readFile "inputs/Day07_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 95437
      solve1 (parse input) @?= 1743217
      solve2 (parse exampleInput) @?= Just 24933642
      solve2 (parse input) @?= Just 8319096
