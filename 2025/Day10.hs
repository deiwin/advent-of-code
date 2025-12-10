module Day10 (main) where

import Control.Applicative ((<|>))
import Data.Bits (setBit, xor, zeroBits)
import Data.Char qualified as C
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.Maybe (fromJust)
import Data.SBV (OptimizeResult (LexicographicResult), OptimizeStyle (Lexicographic), constrain, getModelValue, literal, minimize, optimize, sInteger, (.==), (.>=))
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

type Machine = (String, [[Int]], [Int])

parse :: String -> [Machine]
parse input = run (machine `P.endBy1` eol)
  where
    machine = do
      a <- lights <* spaces
      b <- button `P.endBy1` spaces
      c <- joltages
      return (a, b, c)
    lights = P.char '[' *> P.many1 light <* P.char ']'
    light = P.char '.' <|> P.char '#'
    button = P.char '(' *> (number `P.sepBy1` P.char ',') <* P.char ')'
    joltages = P.char '{' *> (number `P.sepBy1` P.char ',') <* P.char '}'
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    spaces = P.many1 (P.char ' ')
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

lightToI :: String -> Int
lightToI s = buttonToI $ map snd $ filter ((== '#') . fst) $ zip s [0 ..]

buttonToI :: [Int] -> Int
buttonToI = foldl' setBit zeroBits

leastButtonCount :: Machine -> Int
leastButtonCount (lights, buttons, _) = go (Seq.fromList ((0,lightI,) <$> buttonIs))
  where
    lightI = lightToI lights
    buttonIs = buttonToI <$> buttons
    go ((n, 0, _) :<| _) = n
    go ((n, light, button) :<| rest) = go (rest >< Seq.fromList ((n + 1,light `xor` button,) <$> buttonIs))

solve1 :: [Machine] -> Int
solve1 input =
  input
    <&> leastButtonCount
    & sum

leastButtonCount' :: Machine -> _
leastButtonCount' (_, buttons, joltages) = optimize Lexicographic $ do
  vars <- traverse (\i -> sInteger ("button" <> show i <> "PressCount")) $ take (length buttons) [0 ..]
  traverse_ (constrain . (.>= 0)) vars
  traverse_ (constrainJoltage vars) $ zip joltages [0 ..]
  minimize "res" (sum vars)
  where
    constrainJoltage vars (joltage, i) =
      zip vars buttons
        & filter (\(_, b) -> i `elem` b)
        <&> fst
        & sum
        & (.== literal (fromIntegral joltage))
        & constrain

solve2 :: [Machine] -> IO Integer
solve2 input =
  input
    & traverse leastButtonCount'
    <&> sum . fmap (fromJust . extract)
  where
    extract (LexicographicResult model) = getModelValue "res" model

main = do
  input <- readFile "inputs/Day10.txt"
  exampleInput <- readFile "inputs/Day10_example.txt"
  runTestTT $
    TestCase $ do
      buttonToI <$> [[], [0, 1], [2], [0, 2]] @?= [0, 3, 4, 5]
      lightToI <$> ["...", "##.", "..#...", "#.#"] @?= [0, 3, 4, 5]

      solve1 (parse exampleInput) @?= 7
      solve1 (parse input) @?= 452

      res2Example <- solve2 $ parse exampleInput
      res2Example @?= 33

      res2 <- solve2 $ parse input
      res2 @?= 17424
