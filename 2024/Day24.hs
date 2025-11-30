module Day24 (main) where

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Data.Bits (setBit, testBit)
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.Graph.Inductive.Graph (Node, mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz (Attributes, GraphvizParams (fmtEdge, fmtNode), Labellable, defaultParams, graphToDot, toLabel)
import Data.GraphViz.Attributes.Complete (Attribute (..), Color (..), StyleItem (..), StyleName (..), toColorList)
import Data.GraphViz.Commands.IO (writeDotFile)
import Data.List (foldl')
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import System.Random (getStdGen, randoms)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

data Op = AND | OR | XOR
  deriving (Show, Eq)

data Gate = Gate
  { op :: Op,
    a :: String,
    b :: String,
    c :: String
  }
  deriving (Show, Eq)

swap :: String -> String -> [Gate] -> [Gate]
swap a b gates = go <$> gates
  where
    go g
      | g.c == a = g {c = b}
      | g.c == b = g {c = a}
      | otherwise = g

parse :: String -> ([(String, Bool)], [Gate])
parse input = run $ do
  vals <- val `P.endBy1` eol <* eol
  gates <- gate `P.endBy1` eol
  return (vals, gates)
  where
    val = do
      name <- P.many1 alphaNum <* P.string ": "
      state <- (== 1) <$> number
      return (name, state)
    gate = do
      a <- P.many1 alphaNum <* spaces
      op <- ((P.string "AND" $> AND) <|> (P.string "OR" $> OR) <|> (P.string "XOR" $> XOR)) <* spaces
      b <- P.many1 alphaNum <* spaces <* P.string "->" <* spaces
      c <- P.many1 alphaNum
      return $ Gate op a b c
    alphaNum = P.satisfy C.isAlphaNum
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    spaces = P.many1 (P.char ' ')
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

loeb :: (Functor f) => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

bsToI :: [Bool] -> Int
bsToI bs =
  bs
    & zip [0 ..]
    & filter snd
    & foldl' (\acc (i, _) -> acc `setBit` i) 0

prepare :: ([(String, Bool)], [Gate]) -> Map String (Map String Bool -> Bool)
prepare input =
  valFs ++ gateFs
    & M.fromList
  where
    (vals, gates) = input
    valFs = valToF <$> vals
    valToF (name, state) = (name, const state)
    gateFs = gateToF <$> gates
    gateToF (Gate op a b c) = (c, f)
      where
        f m = case op of
          AND -> (m M.! a) && (m M.! b)
          OR -> (m M.! a) || (m M.! b)
          XOR -> (m M.! a) /= (m M.! b)

solve1 :: ([(String, Bool)], [Gate]) -> Int
solve1 input =
  input
    & prepare
    & loeb
    & M.toList
    & filter (\(c : _, _) -> c == 'z')
    & L.sort
    <&> snd
    & bsToI

toName :: Char -> Int -> String
toName c x =
  if x < 10
    then c : '0' : show x
    else c : show x

getSuspects :: [Bool] -> [Bool] -> [Gate] -> Set String
getSuspects xBits yBits gates =
  wrongZBits
    & concatMap dependsOn
    & S.fromList
  where
    vals =
      (first (toName 'x') <$> zip [0 ..] xBits)
        ++ (first (toName 'y') <$> zip [0 ..] yBits)
    initialDevice = prepare (vals, gates)
    initialPostRun = loeb initialDevice
    zBits = charBits initialPostRun 'z'
    x = bsToI xBits
    y = bsToI yBits
    expectedZ = x + y
    expectedZBits = testBit expectedZ <$> [0 .. 45]
    charBits m c = fmap snd $ L.sort $ filter (\(c' : _, _) -> c' == c) $ M.toList m
    wrongZBits =
      zipWith (==) expectedZBits zBits
        & zip [0 ..]
        & filter (not . snd)
        & fmap (toName 'z' . fst)
    dependents =
      gates
        & fmap (\(Gate _ a b c) -> (c, [a, b]))
        & M.fromList
    dependsOn x = x : concatMap dependsOn nextXs
      where
        nextXs = filter (\(c : _) -> c /= 'x' && c /= 'y') $ fromMaybe [] (dependents M.!? x)

isFixed :: [Bool] -> [Gate] -> [(String, String)] -> Bool
isFixed rBits gates swaps =
  rBitGroups
    & zip (tail rBitGroups)
    <&> (\(x, y) -> getSuspects x y newGates)
    & take 10
    & S.unions
    & S.null
  where
    newGates = (\(Gate op a b c) -> Gate op a b $ M.findWithDefault c c swapM) <$> gates
    rBitGroups = take 45 <$> L.iterate (drop 45) rBits
    swapM = M.fromList $ concatMap (\(a, b) -> [(a, b), (b, a)]) swaps

getAllSuspects :: [Bool] -> [Gate] -> Set String
getAllSuspects rBits gates =
  rBitGroups
    & zip (tail rBitGroups)
    <&> (\(x, y) -> getSuspects x y gates)
    & take 2 -- 30
    -- & L.foldl1' S.intersection
    & L.foldl1' S.union
  where
    rBitGroups = take 45 <$> L.iterate (drop 45) rBits

type G = Gr (Attributes, String) String

toGraph :: [Gate] -> Set String -> G
toGraph gates broken = mkGraph nodes edges
  where
    nodes =
      gates
        & concatMap (\(Gate _ a b c) -> [a, b, c])
        & L.nub
        & L.sort
        & zip [0 ..]
        <&> (\(i, name) -> (i, (attrs name, name)))
    attrs name
      | name `S.member` broken = [red, filled]
      | "z" `L.isPrefixOf` name = [green, filled]
      | "x" `L.isPrefixOf` name = [blue, filled]
      | "y" `L.isPrefixOf` name = [yellow, filled]
      | otherwise = []
    red = FillColor (toColorList [RGB 200 0 0])
    green = FillColor (toColorList [RGB 0 200 0])
    blue = FillColor (toColorList [RGB 0 0 200])
    yellow = FillColor (toColorList [RGB 200 200 0])
    filled = Style [SItem Filled []]
    nodeMap :: Map String Node
    nodeMap = M.fromList ((\(i, (_, name)) -> (name, i)) <$> nodes)
    edges =
      gates
        & concatMap
          ( \(Gate op a b c) ->
              [ (nodeMap M.! c, nodeMap M.! a, show op ++ "1"),
                (nodeMap M.! c, nodeMap M.! b, show op ++ "2")
              ]
          )

gparams :: (Labellable el) => GraphvizParams n (Attributes, String) el Int (Attributes, String)
gparams =
  defaultParams
    { fmtNode = nodeFmt,
      fmtEdge = edgeFmt -- ,
      -- clusterBy = clustBy,
      -- clusterID = clustID
    }
  where
    nodeFmt (_, (attrs, l)) = toLabel l : attrs
    edgeFmt (_, _, l) = [toLabel l]

-- clustBy (n, l@(attrs, nl))
--   | "z" `L.isPrefixOf` nl = C 0 $ N (n, l)
--   | "x" `L.isPrefixOf` nl || "y" `L.isPrefixOf` nl = C 2 $ N (n, l)
--   | otherwise = C 1 $ N (n, l)
-- clustID = Num . Int

fixes =
  [ ("z10", "kmb"),
    ("vdk", "mmf"),
    ("tvp", "z15"),
    ("dpg", "z25")
  ]

solve2 :: String
solve2 =
  fixes
    & concatMap (\(a, b) -> [a, b])
    & L.sort
    & L.intercalate ","

toDotFile gates stdGen fileName =
  getAllSuspects (randoms stdGen) gates
    & toGraph gates
    & graphToDot gparams
    & writeDotFile fileName

main = do
  input <- readFile "inputs/Day24.txt"
  stdGen <- getStdGen

  let (_, gates) = parse input
  toDotFile gates stdGen "day24.original.dot"

  let fixedGates = foldr (uncurry swap) gates fixes
  toDotFile fixedGates stdGen "day24.fixed.dot"

  runTestTT $
    TestCase $ do
      bsToI [False] @?= 0
      bsToI [True] @?= 1
      bsToI [True, True] @?= 3
      solve1 (parse input) @?= 51715173446832
      isFixed (randoms stdGen) (snd $ parse input) fixes @?= True
      solve2 @?= "dpg,kmb,mmf,tvp,vdk,z10,z15,z25"
