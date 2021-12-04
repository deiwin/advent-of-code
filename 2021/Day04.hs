module Day04 (main) where

import qualified Data.Char as C
import Data.Function ((&))
import qualified Data.List as L
import Data.Maybe (fromJust, mapMaybe)
import Data.Ord (comparing)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P

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

parse :: String -> Input
parse input = run $ do
  drawNumbers <- numberList <* P.count 2 eol
  boards <- board `P.sepBy1` P.count 2 eol
  eol *> P.eof
  return Input {..}
  where
    board = row `P.sepBy1` eol
    row = P.optional spaces *> number `P.sepBy1` spaces
    numberList = number `P.sepBy1` P.char ','
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    spaces = P.many1 (P.char ' ')
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

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
