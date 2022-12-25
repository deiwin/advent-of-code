module Day25 (main) where

import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.List as L
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [String]
parse = lines

snafuDigitsToDec :: String -> Int
snafuDigitsToDec str = go 1 $ reverse $ fmap (read . toStr) str
  where
    go _ [] = 0
    go m (x:xs) = (x * m) + go (m * 5) xs
    toStr = \case
      '-' -> "-1"
      '=' -> "-2"
      c -> [c]

decToSnafuDigits :: Int -> String
decToSnafuDigits x =
  x
    & digits
    & L.mapAccumL f 0
    & (\(carry, cs) -> [show carry | carry > 0] ++ reverse cs)
    & concat
  where
    f carry x =
      case carry + x of
        0 -> (0, "0")
        1 -> (0, "1")
        2 -> (0, "2")
        3 -> (1, "=")
        4 -> (1, "-")
        5 -> (1, "0")
        x -> error ("undefined for " <> show x)
    digits x
      | x `div` 5 == 0 = [x `mod` 5]
      | otherwise = (x `mod` 5) : digits (x `div` 5)

solve1 :: [String] -> String
solve1 input =
  input
    <&> snafuDigitsToDec
    & sum
    & decToSnafuDigits

main = do
  input <- readFile "inputs/Day25.txt"
  runTestTT $
    TestCase $ do
      snafuDigitsToDec "1=-0-2" @?= 1747
      decToSnafuDigits 1747 @?= "1=-0-2"
      solve1 (parse input) @?= "2-=2-0=-0-=0200=--21"
