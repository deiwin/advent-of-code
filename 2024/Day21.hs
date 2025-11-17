module Day21 (main) where

import Control.Monad.Memo (MonadMemo, memo, startEvalMemo)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl')
import Data.Map.Strict qualified as M
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [String]
parse = lines

data Key = NumKey | DirKey
  deriving (Show, Eq)

numKey :: Char -> V2 Int
numKey = \case
  '7' -> V2 0 0
  '8' -> V2 0 1
  '9' -> V2 0 2
  '4' -> V2 1 0
  '5' -> V2 1 1
  '6' -> V2 1 2
  '1' -> V2 2 0
  '2' -> V2 2 1
  '3' -> V2 2 2
  '0' -> V2 3 1
  'A' -> V2 3 2
  _ -> error "Invalid numerical key"

dirKey :: Char -> V2 Int
dirKey = \case
  '^' -> V2 0 1
  'A' -> V2 0 2
  '<' -> V2 1 0
  'v' -> V2 1 1
  '>' -> V2 1 2
  _ -> error "Invalid directional key"

key :: Key -> Char -> V2 Int
key = \case
  NumKey -> numKey
  DirKey -> dirKey

convertChar :: Key -> Char -> Char -> String
convertChar k fromC toC =
  if ysFirst
    then yChars ++ xChars ++ "A"
    else xChars ++ yChars ++ "A"
  where
    ysFirst =
      case k of
        NumKey -> fromC `elem` numBottomRow && toC `elem` numLeftCol || x > 0
        DirKey
          | fromC `elem` dirLeftCol && toC `elem` dirTopRow -> False
          | fromC `elem` dirTopRow && toC `elem` dirLeftCol -> True
          | otherwise -> x > 0
    numBottomRow = "0A" :: String
    numLeftCol = "147" :: String
    dirLeftCol = "<" :: String
    dirTopRow = "^A" :: String
    (V2 y x) = key k toC - key k fromC
    yChars = replicate (abs y) yChar
    yChar = if y > 0 then 'v' else '^'
    xChars = replicate (abs x) xChar
    xChar = if x > 0 then '>' else '<'

start = 'A'

convert :: Key -> String -> String
convert k s = zipWith (convertChar k) (start : s) s & concat

convertN 1 = convert NumKey
convertN n = convert DirKey . convertN (n - 1)

toI :: String -> Integer
toI = read

scoreN :: Integer -> String -> Integer
scoreN n initial = fromIntegral (length (convertN n initial)) * toI (init initial)

solve1 :: [String] -> Integer
solve1 input =
  input
    <&> scoreN 3
    & sum

scoreNChar :: (MonadMemo (Integer, Char, Char) Integer m) => (Integer, Char, Char) -> m Integer
scoreNChar (0, _, _) = return 1
scoreNChar (n, fromC, toC) =
  convertChar DirKey fromC toC
    & pairs
    & traverse (\(a, b) -> memo scoreNChar (n - 1, a, b))
    & fmap sum
  where
    pairs xs = zip (start : xs) xs

scoreN' :: (MonadMemo (Integer, Char, Char) Integer m) => Integer -> String -> m Integer
scoreN' n initial =
  zip (start : initial) initial
    & traverse (\(a, b) -> scoreNChar (n, a, b))
    <&> sum

solve2 :: [String] -> Integer
solve2 input =
  input
    & traverse (\s -> fmap (* toI (init s)) $ scoreN' 25 $ convert NumKey s)
    & startEvalMemo
    & sum

pos :: Key -> V2 Int -> Char
pos k p =
  case k of
    NumKey -> numM M.! p
    DirKey -> dirM M.! p
  where
    numM =
      "1234567890A"
        <&> (\c -> (numKey c, c))
        & M.fromList
    dirM =
      "<>^vA"
        <&> (\c -> (dirKey c, c))
        & M.fromList

move :: Char -> V2 Int
move = \case
  '<' -> V2 0 (-1)
  '>' -> V2 0 1
  '^' -> V2 (-1) 0
  'v' -> V2 1 0
  'A' -> V2 0 0
  _ -> error "Invalid move"

emulate :: Key -> String -> String
emulate k s =
  foldl' f (key k start, []) s
    & snd
    & reverse
  where
    f (p, acc) c = (p + move c, if c == 'A' then pos k p : acc else acc)

emulateN :: Int -> String -> String
emulateN 1 = emulate NumKey
emulateN n = emulate NumKey . (!! (n - 1)) . iterate (emulate DirKey)

main = do
  input <- readFile "inputs/Day21.txt"
  exampleInput <- readFile "inputs/Day21_example.txt"
  runTestTT $
    TestCase $ do
      toI (init "029A") @?= 29
      emulate DirKey "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"
        @?= "<A>Av<<AA>^AA>AvAA^A<vAAA>^A"
      emulate DirKey "<A>Av<<AA>^AA>AvAA^A<vAAA>^A" @?= "^A<<^^A>>AvvvA"
      emulate NumKey "^A<<^^A>>AvvvA" @?= "379A"
      emulateN 1 "^A<<^^A>>AvvvA" @?= "379A"
      emulateN 2 "<A>Av<<AA>^AA>AvAA^A<vAAA>^A" @?= "379A"
      emulateN 3 "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A" @?= "379A"
      emulateN 3 "v<<A>>^AvA^Av<<A>>^AAv<A<A>>^AAvAA<^A>Av<A>^AA<A>Av<A<A>>^AAAvA<^A>A" @?= "379A"
      solve1 (parse exampleInput) @?= 126384
      solve1 (parse input) @?= 238078
      solve2 (parse exampleInput) @?= 154115708116294
      solve2 (parse input) @?= 293919502998014
