module Practice.Y2015D01 (main) where

import Data.List (foldl', scanl')
import qualified Data.List as L
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Void (Void)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Prelude hiding (readFile)

parse :: Text -> [Char]
parse input = case P.parse parser "" input of
  Left bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
  Right result -> result
  where
    parser = P.some parens
    parens = P.char '(' <|> P.char ')'

solve1 :: [Char] -> Int
solve1 = foldl' f 0

solve2 :: [Char] -> Maybe Int
solve2 = L.findIndex (< 0) . scanl' f 0

f :: Int -> Char -> Int
f acc '(' = acc + 1
f acc ')' = acc - 1
f _ _ = undefined

main = do
  input <- readFile "Practice/inputs/Y2015D01.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 138
      solve2 (parse input) @?= Just 1771
