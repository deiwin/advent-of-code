module Day02 (main) where

import Control.Applicative (empty)
import Data.List (foldl')
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Void (Void)
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
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

parse :: Text -> [Command]
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
    run p = case P.parse p "" input of
      Left bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
      Right result -> result

solve1 :: [Command] -> Int
solve1 input = mul $ foldl' f (V2 0 0) input
  where
    f acc (Forward, x) = acc + V2 x 0
    f acc (Down, x) = acc + V2 0 x
    f acc (Up, x) = acc - V2 0 x

solve2 :: [Command] -> Int
solve2 input = mul $ fst $ foldl' f (V2 0 0, 0) input
  where
    f (coord, aim) (Forward, x) = (coord + V2 x (aim * x), aim)
    f (coord, aim) (Down, x) = (coord, aim + x)
    f (coord, aim) (Up, x) = (coord, aim - x)

mul :: V2 Int -> Int
mul (V2 x y) = x * y

main = do
  input <- readFile "inputs/Day02.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 1882980
      solve2 (parse input) @?= 1971232560
