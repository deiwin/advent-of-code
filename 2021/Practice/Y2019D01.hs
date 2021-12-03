module Practice.Y2019D01 (main) where

import Control.Arrow ((>>>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Prelude hiding (readFile)

parse :: Text -> [Int]
parse = fmap (read . T.unpack) . T.lines

solve1 :: [Int] -> Int
solve1 input = sum (mass <$> input)

solve2 :: [Int] -> Int
solve2 input = sum (recursiveMass <$> input)

recursiveMass :: Int -> Int
recursiveMass =
  iterate mass
    >>> takeWhile (>= 0)
    >>> tail
    >>> sum

mass :: Int -> Int
mass x = (x `div` 3) - 2

main = do
  input <- readFile "Practice/inputs/Y2019D01.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 3249817
      solve2 (parse input) @?= 4871866
