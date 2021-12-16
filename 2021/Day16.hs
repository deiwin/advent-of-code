module Day16 (main) where

import Control.Applicative ((<|>))
import qualified Data.Char as C
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Numeric (readHex)
import qualified Numeric as N
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P
import Text.Printf (printf)

data PacketValue = Literal Int | Container [Packet]
  deriving (Eq, Show)

data Packet = Packet
  { version :: Int,
    packetType :: Int,
    value :: PacketValue
  }
  deriving (Eq, Show)

parse :: String -> Packet
parse = run packetP . hexToBinary
  where
    run p = fst . longestMatch . P.readP_to_S p
    longestMatch = L.minimumBy (comparing (length . snd))

packetP :: P.ReadP Packet
packetP = do
  version <- binaryN 3
  packetType <- binaryN 3
  value <-
    case packetType of
      4 -> literal
      _ -> operator
  return Packet {..}
  where
    operator = bitLengthOperator <|> subCountOperator
    bitLengthOperator = do
      P.char '0'
      bitLength <- binaryN 15
      bits <- P.count bitLength bit
      return (Container (manyPacketsFull bits))
    manyPacketsFull = fullMatch . P.readP_to_S (P.many packetP)
    subCountOperator = do
      P.char '1'
      subCount <- binaryN 11
      packets <- P.count subCount packetP
      return (Container packets)
    literal = do
      firsts <- P.many (P.char '1' *> P.count 4 bit)
      last <- P.char '0' *> P.count 4 bit
      return (Literal (readBinary (concat (firsts ++ [last]))))
    binaryN :: Int -> P.ReadP Int
    binaryN n = readBinary <$> P.count n bit
    readBinary = fullMatch . N.readInt 2 C.isDigit (read . (: []))
    bit = P.satisfy C.isDigit

hexToBinary :: String -> String
hexToBinary hexString = concatMap (printf "%04b") ints
  where
    ints :: [Int]
    ints = fullMatch . readHex . (: []) <$> hexString

fullMatch :: [(a, [b])] -> a
fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: Packet -> Int
solve1 input = sum $ versions input
  where
    versions Packet {version = v, value = (Literal _)} = [v]
    versions Packet {version = v, value = (Container ps)} = v : concatMap versions ps

solve2 :: Packet -> Int
solve2 = compute
  where
    compute :: Packet -> Int
    compute = \case
      Packet {packetType = 0, value = (Container ps)} -> sum (compute <$> ps)
      Packet {packetType = 1, value = (Container ps)} -> product (compute <$> ps)
      Packet {packetType = 2, value = (Container ps)} -> minimum (compute <$> ps)
      Packet {packetType = 3, value = (Container ps)} -> maximum (compute <$> ps)
      Packet {packetType = 4, value = (Literal x)} -> x
      Packet {packetType = 5, value = (Container [a, b])} -> if compute a > compute b then 1 else 0
      Packet {packetType = 6, value = (Container [a, b])} -> if compute a < compute b then 1 else 0
      Packet {packetType = 7, value = (Container [a, b])} -> if compute a == compute b then 1 else 0
      _ -> undefined

main = do
  input <- readFile "inputs/Day16.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse "8A004A801A8002F478") @?= 16
      solve1 (parse input) @?= 908
      solve2 (parse "C200B40A82") @?= 3
      solve2 (parse "04005AC33890") @?= 54
      solve2 (parse "880086C3E88112") @?= 7
      solve2 (parse "CE00C43D881120") @?= 9
      solve2 (parse "D8005AC2A8F0") @?= 1
      solve2 (parse "F600BC2D8F") @?= 0
      solve2 (parse "9C005AC2F8F0") @?= 0
      solve2 (parse "9C0141080250320F1802104A08") @?= 1
      solve2 (parse input) @?= 10626195124371
