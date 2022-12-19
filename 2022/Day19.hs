module Day19 (main) where

import Control.Applicative (empty, (<|>))
import Control.Arrow (first, second, (>>>))
import Control.Monad (guard)
import Control.Monad.Memo (MemoT, MonadMemo, for2, memo, memol0, memol1, startEvalMemo, startEvalMemoT)
import Criterion.Main
  ( bench,
    defaultMain,
    whnf,
  )
import Data.Array.IArray (Array)
import Data.Array.IArray qualified as A
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor (($>), (<$), (<&>))
import Data.Functor.Identity (Identity (runIdentity))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
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
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
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
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tree (Tree (..))
import Data.Tree qualified as T
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as VU
import Data.Void (Void)
import Debug.Trace
  ( traceShow,
    traceShowId,
  )
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P
import Control.Parallel.Strategies (using, rseq, rdeepseq, parTraversable)

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

-- qualityLevel :: Blueprint -> Int
-- qualityLevel bp = maxGeodes * bp.id

type Income = Map Resource Int

type Stash = Map Resource Int

data State = State
  { timeLeft :: Int,
    income :: Income,
    stash :: Stash
  }
  deriving (Eq, Show)

trigNum :: Int -> Int
trigNum n = (n * (n + 1)) `div` 2

maxGeodes :: Blueprint -> _
maxGeodes bp =
  tree
    & prune overprovisioned
    & pruneWithAcc impossible (resourceCount Geode . stash) max
    -- & fmap (resourceCount Obsidian . stash)
    & fmap snd
    & (`using` parTraversable rseq)
    & maximum
  where
    impossible :: (State, Int) -> Bool
    impossible (State{..}, maxSeen) = maxPossible < maxSeen
      where
        maxPossible =
          resourceCount Geode stash
            + (resourceCount Geode income * timeLeft)
            + trigNum timeLeft
    tree :: Tree State
    tree = unfoldTreeDF (\s -> (s, succ s)) initialState
    -- tree = T.unfoldTree (\s -> (s, succ s)) initialState
    initialState = State 19 (M.singleton Ore 1) M.empty
    succ :: State -> [State]
    succ State {timeLeft = 0} = []
    succ s = afterMining : otherOptions
      where
        afterMining =
          s
            { timeLeft = s.timeLeft - 1,
              stash = M.unionWith (+) s.income s.stash
            }
        otherOptions = buildRobot s <$> possibleRobots
          where
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
                { timeLeft = s.timeLeft - 1,
                  income = M.insertWith (+) robot.minedResource 1 s.income,
                  stash = M.unionWith (+) s.income stash
                }
        canBuildRobot :: Robot -> Bool
        canBuildRobot robot =
          robot.cost
            & all (\(resource, cost) -> resourceCount resource s.stash >= cost)
    label :: Tree a -> a
    label (Node a _) = a
    prune :: (a -> Bool) -> Tree a -> Tree a
    prune p = T.foldTree f
      where
        f a ts = Node a (filter (not . p . label) ts)
    pruneWithAcc :: ((a, s) -> Bool) -> (a -> s) -> (s -> s -> s) -> Tree a -> Tree (a, s)
    pruneWithAcc p sForA combine = T.foldTree f
      where
        f a ts = Node (a, reduce (snd . label <$> ts)) (filter (not . p . label) ts)
          where
            reduce = foldl' combine $ sForA a
    -- pruneWithAcc :: ((a, s) -> Bool) -> (a -> s) -> (s -> s -> s) -> Tree a -> (Tree a, s)
    -- pruneWithAcc p sForA combine = T.foldTree f
    --   where
    --     f :: a -> [(Tree a, s)] -> (Tree a, s)
    --     f a ts = (Node a (filter (not . p . first label) ts), reduce (snd <$> ts))
    --       where
    --         reduce = foldl' combine $ sForA a
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
    unfoldTreeDF :: (b -> (a, [b])) -> b -> Tree a
    unfoldTreeDF f = runIdentity . T.unfoldTreeM (return . f)

    resourceCount :: Resource -> Map Resource Int -> Int
    resourceCount = M.findWithDefault 0

solve1 :: _
solve1 input =
  input
    & head
    & maxGeodes

solve2 :: _
solve2 input =
  input

main = do
  input <- readFile "inputs/Day19.txt"
  exampleInput <- readFile "inputs/Day19_example.txt"
  print $ solve1 $ parse exampleInput
  -- print $ solve2 $ parse input
  runTestTT $
    TestCase $ do
      1 @?= 2
      1 @?= 1

-- solve1 (parse input) @?= 2
-- solve2 (parse input) @?= 1
