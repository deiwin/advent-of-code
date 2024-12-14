module Day13 (main) where

import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.Maybe (fromJust)
import Data.PQueue.Min (MinQueue (..))
import Data.PQueue.Min qualified as PQ
import Data.Set (Set)
import Data.Set qualified as S
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

parse :: String -> [(V2 Integer, V2 Integer, V2 Integer)]
parse input = run (game `P.sepBy1` eol)
  where
    game = do
      a <- button "A" <* eol
      b <- button "B" <* eol
      prize <- prize <* eol
      return (a, b, prize)
    button c = do
      P.string ("Button " <> c <> ": ")
      x <- P.string "X+" *> number <* P.char ',' <* spaces
      y <- P.string "Y+" *> number
      return (V2 y x)
    prize = do
      x <- P.string "Prize: X=" *> number <* P.char ',' <* spaces
      y <- P.string "Y=" *> number
      return (V2 y x)
    -- Standard parsers
    number :: P.ReadP Integer
    number = read <$> P.munch1 C.isDigit
    spaces = P.many1 (P.char ' ')
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

y (V2 y _) = y

x (V2 _ x) = x

simplify :: (V2 Integer, V2 Integer, V2 Integer) -> (V2 Integer, V2 Integer, V2 Integer)
simplify (a, b, prize) =
  ( V2 (y a `quot` gcdY) (x a `quot` gcdX),
    V2 (y b `quot` gcdY) (x b `quot` gcdX),
    V2 (y prize `quot` gcdY) (x prize `quot` gcdX)
  )
  where
    gcdY = gcd (gcd (y a) (y b)) (y prize)
    gcdX = gcd (gcd (x a) (x b)) (x prize)

type Queue = MinQueue (Integer, Integer, V2 Integer)

type Visited = Set (V2 Integer)

type Acc = (Queue, Visited)

cost :: (V2 Integer, V2 Integer, V2 Integer) -> (Bool, Integer)
cost (a, b, prize) = L.last $ L.unfoldr go (PQ.singleton (0, manhattan prize, V2 0 0), S.empty)
  where
    go :: Acc -> Maybe ((Bool, Integer), Acc)
    go (Empty, _) = Nothing
    go ((cost, _, pos) :< xs, visited)
      | pos `S.member` visited = Just ((False, cost), (xs, visited))
      | pos == prize = Just ((True, cost), emptyAcc)
      | otherwise = Just ((False, cost), newAcc)
      where
        emptyAcc = (PQ.empty, S.empty)
        newAcc = (newXs, newVisited)
        newXs = PQ.fromList news <> xs
        news =
          [ (cost + 3, manhattan (prize - newA), newA),
            (cost + 1, manhattan (prize - newB), newB)
          ]
            & filter (\(_, _, pos) -> pos `S.notMember` visited)
            & filter prePrize
        prePrize (_, _, pos) = y pos <= y prize && x pos <= x prize
        newA = pos + a
        newB = pos + b
        newVisited = S.insert pos visited
    manhattan (V2 y x) = abs y + abs x

solve1 :: [(V2 Integer, V2 Integer, V2 Integer)] -> Integer
solve1 input =
  input
    <&> cost . simplify
    & filter fst
    <&> snd
    & sum

cost' :: (V2 Integer, V2 Integer, V2 Integer) -> (Bool, Integer)
cost' (a, b, prize) = (approx == prize, mulA * 3 + mulB)
  where
    approx = return mulA * a + return mulB * b
    mulA = round $ lenA / len a
    mulB = round $ lenB / len b
    lenA = lenP * sin degA / sin degP
    lenB = lenA * sin degB / sin degA
    lenP = len prize
    len v = sqrt $ fromIntegral $ y v ^ 2 + x v ^ 2
    atanA = atan a
    atanB = atan b
    atanP = atan prize
    atan v = atan2 (fromIntegral $ y v) (fromIntegral $ x v)
    degA = pi - degB - degP
    degB = abs $ atanA - atanP
    degP =
      if atanA >= atanP
        then pi - atanA + atanB
        else pi - atanB + atanA

solve2 :: [(V2 Integer, V2 Integer, V2 Integer)] -> Integer
solve2 input =
  input
    <&> cost' . grow
    & filter fst
    <&> snd
    & sum
  where
    grow (a, b, prize) = (a, b, V2 (y prize + 10000000000000) (x prize + 10000000000000))

main = do
  input <- readFile "inputs/Day13.txt"
  exampleInput <- readFile "inputs/Day13_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 480
      solve1 (parse input) @?= 40369
      solve2 (parse exampleInput) @?= 875318608908
      solve2 (parse input) @?= 72587986598368
