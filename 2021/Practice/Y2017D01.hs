module Practice.Y2017D01 (main) where

import Data.Function ((&))
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Void (Void)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Prelude hiding (readFile)

parse :: Text -> [Int]
parse input = run parser
  where
    parser = fmap (read . (: [])) <$> P.some P.digitChar
    run p = case P.parse p "" input of
      Left bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
      Right result -> result

solve1 :: [Int] -> Int
solve1 input =
  input
    & zip (tail input ++ [head input])
    & filter (uncurry (==))
    & fmap fst
    & sum

solve2 :: [Int] -> Int
solve2 input =
  input
    & zip (drop half input ++ take half input)
    & filter (uncurry (==))
    & fmap fst
    & sum
  where
    half = length input `div` 2

main = do
  input <- readFile "Practice/inputs/Y2017D01.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 1341
      solve2 (parse input) @?= 1348
