module Practice.Y2023D01 (main) where

import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [String]
parse = lines

toI :: String -> Int
toI = read

solve1 :: [String] -> _
solve1 input =
  input
    <&> (toI . (\s -> [head s, L.last s]) . filter C.isDigit)
    & sum

digitNames =
  [ ("zero", 0),
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)
  ]

replaceDigitNames :: String -> String
replaceDigitNames =
  \case
    "" -> ""
    s -> case L.find ((`L.isPrefixOf` s) . fst) digitNames of
      Nothing -> head s : replaceDigitNames (tail s)
      Just (_, i) -> show i ++ replaceDigitNames (tail s)

solve2 :: [String] -> _
solve2 input =
  input
    <&> (toI . (\s -> [head s, L.last s]) . filter C.isDigit . replaceDigitNames)
    & sum

main = do
  input <- readFile "Practice/inputs/Y2023D01.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 54968
      solve2 (parse input) @?= 54094
