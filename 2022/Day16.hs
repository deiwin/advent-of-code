module Day16 (main) where

import Control.Applicative ((<|>))
import Control.Monad.Memo (MonadMemo, memo, startEvalMemo)
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl1')
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

data NodeInfo = NodeInfo
  { name :: String,
    rate :: Int,
    edgesTo :: [String]
  }
  deriving (Show, Eq)

parse :: String -> [NodeInfo]
parse input = run $ nodeInfo `P.endBy` eol
  where
    nodeInfo =
      NodeInfo
        <$> (P.string "Valve " *> name)
        <*> (P.string " has flow rate=" *> number)
        <*> (tunnelText *> (name `P.sepBy1` P.string ", "))
    tunnelText =
      P.string "; tunnels lead to valves "
        <|> P.string "; tunnel leads to valve "
    name = P.count 2 letter
    -- Standard parsers
    letter = P.satisfy C.isLetter
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

type Graph = Map String (Int, [String])

mkGraph :: [NodeInfo] -> Graph
mkGraph = M.fromList . fmap (\ni -> (ni.name, (ni.rate, ni.edgesTo)))

distanceMap :: Graph -> Map (String, String) Int
distanceMap g =
  nonZeroValves
    & ("AA" :)
    <&> toDistanceMap
    & foldl1' M.union
  where
    nonZeroValves = fmap fst $ filter ((> 0) . fst . snd) $ M.toList g
    toDistanceMap :: String -> Map (String, String) Int
    toDistanceMap node =
      node
        & nodeDistanceMap g
        & M.filterWithKey (\k _ -> k `elem` nonZeroValves)
        & M.mapKeys (node,)
    nodeDistanceMap :: Graph -> String -> Map String Int
    nodeDistanceMap g from = go (0, from) Seq.empty (S.singleton from) M.empty
      where
        go :: (Int, String) -> Seq (Int, String) -> Set String -> Map String Int -> Map String Int
        go (steps, node) toVisit explored m =
          case newToVisit of
            Empty -> newM
            x :<| xs -> go x xs newExplored newM
          where
            newM = M.insert node steps m
            newToExplore =
              node
                & (g M.!)
                & snd
                & filter (not . (`S.member` explored))
            newToVisit =
              newToExplore
                <&> (steps + 1,)
                & Seq.fromList
                & (toVisit Seq.><)
            newExplored = S.union explored (S.fromList newToExplore)

data AgentState = AgentState
  { timeLeft :: Int,
    node :: String
  }
  deriving (Eq, Show, Ord)

maxPressure :: [AgentState] -> Graph -> Int
maxPressure agents g = startEvalMemo $ goAll (agents, S.fromList nonZeroValves)
  where
    nonZeroValves = fmap fst $ filter ((> 0) . fst . snd) $ M.toList g
    goAll :: MonadMemo (AgentState, Set String) Int m => ([AgentState], Set String) -> m Int
    goAll ([a], closedValves) = go (a, closedValves)
    goAll ([a, b], closedValves) =
      allSplits
        & mapM pressureForSplit
        <&> maximum
      where
        pressureForSplit (s1, s2) = do
          x <- go (a, s1)
          y <- go (b, s2)
          return (x + y)
        allSplits =
          closedValves
            & S.toList
            & L.subsequences
            <&> (\s -> (s, S.difference closedValves s)) . S.fromList
    goAll _ = undefined
    go :: MonadMemo (AgentState, Set String) Int m => (AgentState, Set String) -> m Int
    go (!agent, !closedValves)
      | L.null moveOptions = return newPressure
      | newPressure == 0 =
          moveOptions
            & mapM (\newAgent -> memo go (newAgent, newClosedValves newAgent))
            <&> (+ newPressure) . maximum
      | otherwise =
          moveOptions
            & mapM (\newAgent -> memo go (newAgent, newClosedValves newAgent))
            <&> (+ newPressure) . maximum
      where
        newClosedValves agent = S.delete agent.node closedValves
        newPressure = agent.timeLeft * fst (g M.! agent.node)
        moveOptions :: [AgentState]
        moveOptions =
          closedValves
            & S.toList
            <&> move
            & filter ((> 0) . timeLeft)
        move to =
          agent
            { timeLeft = agent.timeLeft - distance agent.node to - 1,
              node = to
            }
    distance from to = distMap M.! (from, to)
    distMap = distanceMap g

solve1 :: [NodeInfo] -> Int
solve1 input =
  input
    & mkGraph
    & maxPressure [AgentState 30 "AA"]

solve2 :: [NodeInfo] -> Int
solve2 input =
  input
    & mkGraph
    & maxPressure [AgentState 26 "AA", AgentState 26 "AA"]

main = do
  input <- readFile "inputs/Day16.txt"
  exampleInput <- readFile "inputs/Day16_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 1651
      solve1 (parse input) @?= 1460
      solve2 (parse exampleInput) @?= 1707
      solve2 (parse input) @?= 2117
