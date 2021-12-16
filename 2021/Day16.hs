{-# LANGUAGE TypeApplications #-}

module Day16 (main) where

import Control.Applicative (empty, (<|>))
import Control.Arrow (second, (>>>))
import Control.Monad (guard)
import Criterion.Main
  ( bench,
    defaultMain,
    whnf,
  )
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import qualified Data.Char as C
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
    fromJust,
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
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import Data.Void (Void)
import Debug.Trace
  ( traceShow,
    traceShowId,
  )
import GHC.Show (intToDigit)
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import Numeric (readHex, showIntAtBase)
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

parse :: String -> _
parse input = fst $ run packet
  where
    packet = do
      version <- binaryN 3
      packetType <- binaryN 3
      value <-
        case packetType of
          4 -> literal
          _ -> operator
      return Packet {..}
    operator :: P.ReadP PacketValue
    operator = bitLengthOperator <|> subCountOperator
    bitLengthOperator = do
      P.char '0'
      bitLength <- binaryN 15
      bits <- P.count bitLength bit
      return (Container (manyPacketsFull bits))
    manyPacketsFull = fullMatch . P.readP_to_S (P.many packet)
    subCountOperator = do
      P.char '1'
      subCount <- binaryN 11
      packets <- P.count subCount packet
      return (Container packets)
    -- manyPacketsFull = fullMatch . P.readP_to_S (P.many packet)
    countPacketsFull n = fullMatch . P.readP_to_S (P.count n packet)
    literal :: P.ReadP PacketValue
    literal = do
      firsts <- P.many (P.char '1' *> P.count 4 bit)
      last <- P.char '0' *> P.count 4 bit
      return (Literal (readBinary (concat (firsts ++ [last]))))
    binaryN :: Int -> P.ReadP Int
    binaryN n = readBinary <$> P.count n bit
    readBinary = fullMatch . N.readInt 2 C.isDigit (read . (: []))
    bit :: P.ReadP Char
    bit = P.satisfy C.isDigit
    ints :: [Int]
    ints = fullMatch . readHex . (: []) <$> input
    binaryString :: String
    binaryString = concatMap (printf "%04b") ints
    -- Standard parsers
    letter = P.satisfy C.isLetter
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    spaces = P.many1 (P.char ' ')
    eol = P.char '\n'
    run p = longestMatch $ P.readP_to_S p binaryString
    longestMatch :: [(a, [b])] -> (a, [b])
    longestMatch = L.minimumBy (comparing (length . snd))
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: _
solve1 input = sum $ versions input
  where
    versions Packet {version = v, value = (Literal _)} = [v]
    versions Packet {version = v, value = (Container ps)} = v : concatMap versions ps

solve2 :: _
solve2 = compute
  where
    compute :: Packet -> Int
    compute Packet {packetType = 0, value = (Container ps)} = sum (compute <$> ps)
    compute Packet {packetType = 1, value = (Container ps)} = product (compute <$> ps)
    compute Packet {packetType = 2, value = (Container ps)} = minimum (compute <$> ps)
    compute Packet {packetType = 3, value = (Container ps)} = maximum (compute <$> ps)
    compute Packet {packetType = 4, value = (Literal x)} = x
    compute Packet {packetType = 5, value = (Container [a, b])} = if compute a > compute b then 1 else 0
    compute Packet {packetType = 6, value = (Container [a, b])} = if compute a < compute b then 1 else 0
    compute Packet {packetType = 7, value = (Container [a, b])} = if compute a == compute b then 1 else 0
    compute _ = undefined

main = do
  input <- readFile "inputs/Day16.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse "8A004A801A8002F478") @?= 16
      solve1 (parse input) @?= 908
      solve2 (parse "C200B40A82") @?= 3
      solve2 (parse input) @?= 10626195124371
