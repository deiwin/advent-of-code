module Practice.Y2016D01 (main) where

import Control.Applicative (empty)
import Data.List (foldl')
import qualified Data.List as L
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Void (Void)
import Linear.V2 (V2 (..), perp)
import Test.HUnit.Base
  ( Test (TestCase),
    (@?=),
  )
import Test.HUnit.Text (runTestTT)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PL
import Prelude hiding (readFile)

type Parser = P.Parsec Void Text

data Turn = L | R
  deriving (Read, Show)

parse :: Text -> [(Turn, Int)]
parse input = case P.parse parser "" input of
  Left bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
  Right result -> result
  where
    parser = instruction `P.sepBy1` string ","
    instruction = do
      turn :: Turn <- read . (:[]) <$> P.letterChar
      count <- number
      return (turn, count)
    spaceConsumer :: Parser ()
    spaceConsumer = PL.space (P.skipSome (P.char ' ')) empty empty
    lexeme = PL.lexeme spaceConsumer
    string = PL.symbol spaceConsumer
    number = lexeme PL.decimal

solve1 :: [(Turn, Int)] -> Int
solve1 input = dist $ snd $ foldl' f (V2 0 1, V2 0 0) input
  where
    f (dir, pos) (turn, x) = (newDir, pos + (newDir * return x))
      where
        newDir = case turn of
          L -> left dir
          R -> right dir
        left = perp
        right = left . left . left

solve2 :: [(Turn, Int)] -> Int
solve2 input = dist $ firstMatch $ reverse points
  where
    firstMatch (x : xs)
      | x `L.elem` xs = x
      | otherwise = firstMatch xs
    firstMatch [] = undefined
    points = snd $ foldl' f (V2 0 1, [V2 0 0]) input
    f (_, []) (_, _) = undefined
    f (dir, poss@(pos : _)) (turn, x) = (newDir, newPoss ++ poss)
      where
        newPoss = reverse $ tail $ L.scanl' (+) pos $ replicate x newDir
        newDir = case turn of
          L -> left dir
          R -> right dir
        left = perp
        right = left . left . left

dist :: V2 Int -> Int
dist = sum . fmap abs

main = do
  input <- readFile "Practice/inputs/Y2016D01.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 253
      solve2 (parse input) @?= 126
