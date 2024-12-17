module Day17 (main) where

import Data.Bits (shiftR, xor, (.&.))
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Maybe (fromJust)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

parse :: String -> _
parse input = run $ do
  a <- P.string "Register A: " *> number <* eol
  b <- P.string "Register B: " *> number <* eol
  c <- P.string "Register C: " *> number <* eol <* eol
  program <- IM.fromList . zip [0 ..] <$> (P.string "Program: " *> number `P.sepBy1` P.char ',' <* eol)
  return $ State a b c program 0 []
  where
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

data State = State
  { a :: Int,
    b :: Int,
    c :: Int,
    program :: IntMap Int,
    pointer :: Int,
    output :: [Int]
  }
  deriving (Show)

step :: State -> Maybe State
step s@State {}
  | s.pointer >= IM.size s.program = Nothing
  | s.pointer == IM.size s.program - 1 = error "Invalid program"
  | otherwise = Just newS
  where
    instruction = s.program IM.! s.pointer
    operand = s.program IM.! (s.pointer + 1)
    combo =
      case operand of
        i | i <= 3 -> i
        4 -> s.a
        5 -> s.b
        6 -> s.c
        _ -> error "Invalid combo operand"
    newS =
      case instruction of
        0 -> s {a = s.a `shiftR` combo} & jump
        1 -> s {b = s.b `xor` operand} & jump
        2 -> s {b = combo .&. 7} & jump
        3 -> if s.a == 0 then jump s else s {pointer = operand}
        4 -> s {b = s.b `xor` s.c} & jump
        5 -> s {output = (combo .&. 7) : s.output} & jump
        6 -> s {b = s.a `shiftR` combo} & jump
        7 -> s {c = s.a `shiftR` combo} & jump
        _ -> error "Invalid instruction"
    jump s = s {pointer = s.pointer + 2}

runProgram :: State -> [Int]
runProgram s =
  L.unfoldr (fmap (\x -> (x, x)) . step) s
    & last
    & output
    & reverse

solve1 :: State -> [Int]
solve1 = runProgram

solve2 :: State -> Int
solve2 input =
  go [0]
  where
    go :: [Int] -> Int
    go xs
      | Just (x, _) <- L.find ((== initialProgram) . snd) next = x
      | otherwise = go $ fst <$> next
      where
        next =
          xs
            & concatMap grow
            <&> (\a -> (a, runProgram (input {a = a})))
            & filter ((`L.isSuffixOf` initialProgram) . snd)
        grow x = ((x * 8) +) <$> [0 .. 7]
    initialProgram = IM.elems input.program

main = do
  input <- readFile "inputs/Day17.txt"
  exampleInput <- readFile "inputs/Day17_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= [5, 7, 3, 0]
      solve1 (parse input) @?= [2, 7, 4, 7, 2, 1, 7, 5, 1]
      solve2 (parse exampleInput) @?= 117440
      solve2 (parse input) @?= 37221274271220
