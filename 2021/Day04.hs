module Day04 (main) where

import Control.Applicative (empty)
import Data.Function ((&))
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Void (Void)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PL
import Prelude hiding (readFile)

type Parser = P.Parsec Void Text

type Board = [[Int]]

data Input = Input
  { drawNumbers :: [Int],
    boards :: [Board]
  }
  deriving (Show, Eq)

data BoardScore = BoardScore
  { score :: Int,
    calledNumbers :: [Int]
  }
  deriving (Show, Eq)

parse :: Text -> Input
parse input = run parser
  where
    parser = do
      drawNumbers <- number `P.sepEndBy1` P.char ',' <* P.space
      boards <- boardP `P.sepEndBy1` P.space
      return Input {..}
    boardP = rowP `P.sepBy1` P.try singleEol
    rowP = P.space *> P.some number
    number = lexeme PL.decimal
    lexeme = PL.lexeme spaceConsumer
    spaceConsumer :: Parser ()
    spaceConsumer = PL.space (P.skipSome (P.char ' ')) empty empty
    singleEol :: Parser (P.Tokens Text)
    singleEol = P.eol <* P.notFollowedBy P.eol
    run p = case P.parse p "" input of
      Left bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
      Right result -> result

solve1 :: Input -> Int
solve1 input =
  boards input
    & mapMaybe (boardScore (drawNumbers input))
    & L.minimumBy (comparing (length . calledNumbers))
    & score

solve2 :: Input -> Int
solve2 input =
  boards input
    & mapMaybe (boardScore (drawNumbers input))
    & L.maximumBy (comparing (length . calledNumbers))
    & score

boardScore :: [Int] -> Board -> Maybe BoardScore
boardScore drawNumbers rows = do
  calledNumbers <- calledNumbersM
  let score = unmarkedNumberSum calledNumbers * lastCalledNumber calledNumbers
  return BoardScore {..}
  where
    columns = L.transpose rows
    calledNumbersM :: Maybe [Int]
    calledNumbersM =
      traverse (lineCalledNumbers drawNumbers) (rows ++ columns)
        & fmap (L.minimumBy (comparing length))
    unmarkedNumberSum calledNumbers = sum $ filter (not . flip L.elem calledNumbers) $ concat rows
    lastCalledNumber calledNumbers = L.last calledNumbers

lineCalledNumbers :: [Int] -> [Int] -> Maybe [Int]
lineCalledNumbers drawNumbers line =
  L.scanl' eliminate line drawNumbers
    & L.findIndex L.null
    & fmap (`take` drawNumbers)
  where
    eliminate xs x = filter (/= x) xs

main = do
  input <- readFile "inputs/Day04.txt"
  exampleInput <- readFile "inputs/Day04_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 4512
      solve1 (parse input) @?= 16674
      solve2 (parse exampleInput) @?= 1924
      solve2 (parse input) @?= 7075
