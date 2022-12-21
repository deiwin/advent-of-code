module Day19 (main) where

import Control.Applicative ((<|>))
import Control.Parallel.Strategies (parMap, rseq)
import Data.Char qualified as C
import Data.Containers.ListUtils (nubOrd)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl')
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Debug.Trace (traceShow)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P

data Resource = Ore | Clay | Obsidian | Geode
  deriving (Eq, Show, Ord)

data Robot = Robot
  { minedResource :: Resource,
    cost :: [(Resource, Int)]
  }
  deriving (Eq, Show)

data Blueprint = Blueprint
  { id :: Int,
    robots :: [Robot]
  }
  deriving (Eq, Show)

parse :: String -> [Blueprint]
parse input = run $ blueprint `P.endBy` eol
  where
    blueprint =
      Blueprint
        <$> (P.string "Blueprint " *> number <* P.string ": ")
        <*> (robot `P.sepBy` P.string ". ")
        <* P.char '.'
    robot =
      Robot
        <$> (P.string "Each " *> resource <* P.string " robot costs ")
        <*> (cost `P.sepBy` P.string " and ")
    resource =
      Ore <$ P.string "ore"
        <|> Clay <$ P.string "clay"
        <|> Obsidian <$ P.string "obsidian"
        <|> Geode <$ P.string "geode"
    cost = flip (,) <$> (number <* P.char ' ') <*> resource
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

type Income = Map Resource Int

type Stash = Map Resource Int

data State = State
  { income :: Income,
    stash :: Stash
  }
  deriving (Eq, Show, Ord)

maxGeodes :: Int -> Blueprint -> Int
maxGeodes timeAvailable bp = go timeAvailable S.empty [initialState]
  where
    go timeLeft seen states
      | timeLeft == 0 = maxSoFar
      | otherwise = go (timeLeft - 1) newSeen newStates
      where
        maxSoFar = maximum (resourceCount Geode . stash <$> states)
        newSeen = S.union seen (S.fromList (income <$> newStates))
        newStates = justMineStates ++ buildStates
        justMineStates = mine <$> states
        buildStates =
          states
            & concatMap (nextStateOptions bp timeLeft maxSoFar)
            & nubOrd
            & filter ((`S.notMember` seen) . income)
            & traceShow timeLeft
    initialState = State (M.singleton Ore 1) M.empty

nextStateOptions :: Blueprint -> Int -> Int -> State -> [State]
nextStateOptions bp timeLeft maxSoFar s =
  possibleRobots
    <&> buildRobot s
    & filter (not . overprovisioned)
    & filter (not . impossible)
  where
    overprovisioned :: State -> Bool
    overprovisioned State {income} = f Ore || f Clay || f Obsidian
      where
        f r = M.findWithDefault 0 r income > g r
        g r =
          bp.robots
            & concatMap cost
            & filter ((== r) . fst)
            <&> snd
            & maximum
    impossible :: State -> Bool
    impossible s = maxPossible < maxSoFar
      where
        maxPossible =
          resourceCount Geode s.stash
            + (resourceCount Geode s.income * timeLeft)
            + trigNum timeLeft
    trigNum :: Int -> Int
    trigNum n = (n * (n + 1)) `div` 2
    possibleRobots :: [Robot]
    possibleRobots = filter canBuildRobot bp.robots
    buildRobot :: State -> Robot -> State
    buildRobot s robot =
      robot.cost
        & foldl' (\stash (resource, cost) -> M.update (\x -> Just (x - cost)) resource stash) s.stash
        & update
      where
        update stash =
          State
            { income = M.insertWith (+) robot.minedResource 1 s.income,
              stash = M.unionWith (+) s.income stash
            }
    canBuildRobot :: Robot -> Bool
    canBuildRobot robot =
      robot.cost
        & all (\(resource, cost) -> resourceCount resource s.stash >= cost)

mine :: State -> State
mine s = s {stash = M.unionWith (+) s.income s.stash}

resourceCount :: Resource -> Map Resource Int -> Int
resourceCount = M.findWithDefault 0

solve1 :: [Blueprint] -> Int
solve1 input =
  input
    & parMap rseq (maxGeodes 24)
    & zip input
    <&> (\(bp, maxGeodes) -> bp.id * maxGeodes)
    & sum

solve2 :: [Blueprint] -> Int
solve2 input =
  input
    & take 3
    & parMap rseq (maxGeodes 32)
    & product

main = do
  input <- readFile "inputs/Day19.txt"
  exampleInput <- readFile "inputs/Day19_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 33
      solve1 (parse input) @?= 1466
      solve2 (parse exampleInput) @?= 3472
      solve2 (parse input) @?= 8250
