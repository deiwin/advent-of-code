module Day21 (main) where

import Control.Applicative ((<|>))
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.Maybe (catMaybes, fromJust, listToMaybe)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

data Operator = Add | Subtract | Multiply | Divide
  deriving (Eq, Show)

data Value
  = Val Int
  | Op
      { lhs :: String,
        operator :: Operator,
        rhs :: String
      }
  deriving (Eq, Show)

data Assignment = Assignment
  { name :: String,
    value :: Value
  }
  deriving (Eq, Show)

parse :: String -> [Assignment]
parse input = run $ assignment `P.endBy` eol
  where
    assignment = Assignment <$> (word <* P.string ": ") <*> value
    value = val <|> op
    val = Val <$> number
    op =
      Op
        <$> (word <* P.char ' ')
        <*> (operator <* P.char ' ')
        <*> word
    operator =
      Add <$ P.char '+'
        <|> Subtract <$ P.char '-'
        <|> Multiply <$ P.char '*'
        <|> Divide <$ P.char '/'
    -- Standard parsers
    word = P.many1 letter
    letter = P.satisfy C.isLetter
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

process :: HashMap String Value -> HashMap String Int
process input =
  input
    <&> toField
    & loeb
  where
    toField (Val x) _ = x
    toField op m = (m HM.! op.lhs) `f` (m HM.! op.rhs)
      where
        f = case op.operator of
          Add -> (+)
          Subtract -> (-)
          Multiply -> (*)
          Divide -> div

solve1 :: [Assignment] -> Int
solve1 input =
  input
    <&> (\a -> (a.name, a.value))
    & HM.fromList
    & process
    & (HM.! "root")

bSearch :: Int -> Int -> (Int -> Ordering) -> Maybe Int
bSearch min max p
  | min >= max = Nothing
  | otherwise =
      case p midPoint of
        -- Find the smallest out of multiple fitting values
        EQ ->
          [ bSearch 0 (midPoint `div` 100) p,
            bSearch 0 (midPoint `div` 10) p,
            bSearch min (midPoint - 1) p,
            Just midPoint
          ]
            & catMaybes
            & listToMaybe
        LT -> bSearch min (midPoint - d) p
        GT -> bSearch (midPoint + d) max p
  where
    d = if max - min == 1 then 1 else 0
    midPoint = ((max - min) `div` 2) + min

solve2 :: [Assignment] -> Maybe Int
solve2 input = bSearch 0 maxBound p
  where
    p :: Int -> Ordering
    p x =
      withoutRoot
        & HM.insert "humn" (Val x)
        & process
        & (\m -> compare (m HM.! lhs) (m HM.! rhs))
    inputMap = HM.fromList ((\a -> (a.name, a.value)) <$> input)
    (Op {lhs, rhs}) = inputMap HM.! "root"
    withoutRoot = HM.delete "root" inputMap

main = do
  input <- readFile "inputs/Day21.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 256997859093114
      solve2 (parse input) @?= Just 3952288690726
