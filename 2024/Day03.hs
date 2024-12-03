module Day03 (main) where

import Control.Applicative ((<|>))
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.Maybe (catMaybes, fromJust)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

data Op = Mul Int Int | Enable | Disable
  deriving (Show)

parse :: String -> [Op]
parse input = catMaybes $ run $ P.many1 opM
  where
    -- Standard parsers
    opM = (Just <$> op) P.<++ (Nothing <$ P.get)
    op = mul <|> enable <|> disable
    enable = Enable <$ P.string "do()"
    disable = Disable <$ P.string "don't()"
    mul = do
      P.string "mul" <* P.char '('
      x <- number <* P.char ','
      y <- number <* P.char ')'
      return $ Mul x y
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: [Op] -> Int
solve1 input =
  input
    <&> (\case (Mul x y) -> x * y; _ -> 0)
    & sum

solve2 :: [Op] -> Int
solve2 input =
  input
    & L.scanl' f (True, 0)
    & filter fst
    <&> snd
    & sum
  where
    f (True, _) (Mul x y) = (True, x * y)
    f (False, _) (Mul _ _) = (False, 0)
    f _ Enable = (True, 0)
    f _ Disable = (False, 0)

main = do
  input <- readFile "inputs/Day03.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 188192787
      solve2 (parse input) @?= 113965544
