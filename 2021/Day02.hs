module Day02 (main) where

import Control.Applicative (empty)
import Control.Arrow (second, (>>>))
import Control.Monad (guard)
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

data Direction = Forward | Down | Up
  deriving (Show, Eq, Read)

type Command = (Direction, Int)

parse :: Text -> _
parse input = run parser
  where
    parser = commandParser `P.sepEndBy1` P.space
    commandParser = do
      dir <- dirParser
      numbers <- number
      return (dir, numbers)
    dirParser =
      Forward <$ string "forward"
        <|> Down <$ string "down"
        <|> Up <$ string "up"
    string = PL.symbol spaceConsumer
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
solve1 input = mul $ fst $ foldl' f (V2 0 0, 0) input
  where
    mul (V2 x y) = x * y
    f (coord, aim) (Forward, x) = (coord + V2 x (aim * x), aim)
    f (coord, aim) (Down, x) = (coord, aim + x)
    f (coord, aim) (Up, x) = (coord, aim - x)

main = do
  input <- readFile "inputs/Day02.txt"
  -- exampleInput <- readFile "inputs/Day02_example.txt"
  print $ solve1 $ parse input
  runTestTT $
    TestCase $ do
      1 @?= 2
      1 @?= 1
