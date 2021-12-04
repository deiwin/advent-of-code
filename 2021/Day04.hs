module Day04 (main) where

import Control.Applicative (empty)
import Control.Arrow (second, (>>>))
import Control.Monad (guard, liftM)
import Criterion.Main
  ( bench,
    defaultMain,
    whnf,
  )
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
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
    isJust,
    mapMaybe,
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
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile)
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
import Test.HUnit.Base
  ( Test (TestCase),
    (@?=),
  )
import Test.HUnit.Text (runTestTT)
import Text.Megaparsec ((<|>))
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
      -- row <- boardP
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

solve1 :: _
solve1 input =
  boards input
    & mapMaybe (boardScore (drawNumbers input))
    & L.minimumBy (comparing (length . calledNumbers))
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
lineCalledNumbers drawNumbers line = do
  i <-
    L.scanl' eliminate line drawNumbers
      & L.findIndex L.null
  return $ take i drawNumbers
  where
    eliminate xs x = filter (/= x) xs

main = do
  input <- readFile "inputs/Day04.txt"
  exampleInput <- readFile "inputs/Day04_example.txt"
  print $ solve1 $ parse exampleInput
  print $
    lineCalledNumbers
      [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1]
      [14, 21, 17, 24, 4]
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 4512
      solve1 (parse input) @?= 16674
