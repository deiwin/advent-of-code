module Day19 (main) where

import Control.Applicative (empty, (<|>))
import Control.Arrow (first, second, (>>>))
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
import Data.Functor ((<$), (<&>), ($>))
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
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P
import Control.Monad.Memo (MemoT, MonadMemo, for2, memo, startEvalMemoT, startEvalMemo, memol0, memol1)
import Data.Functor.Identity (Identity)

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
        <*> (robot `P.sepBy` P.string ". ") <* P.char '.'
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

type Memo0 = MemoT (Int, (Income, Stash)) Stash
type Memo1 = MemoT Stash [[Robot]]
type MemoAll = Memo0 (Memo1 Identity)

maxGeodes :: Blueprint -> _
-- maxGeodes bp = geodeCount $ go 24 (M.singleton Ore 1) M.empty
maxGeodes bp = startEvalMemo $ startEvalMemoT $ go 24 (M.singleton Ore 1, M.empty)
  where
    go :: Int -> (Income, Stash) -> MemoAll Stash
    go timeLeft (income, stash)
      | traceShow (timeLeft, stash) False = undefined
      | timeLeft == 0 = return stash
      | otherwise = L.maximumBy (comparing geodeCount) <$> options
      where
        options = do
          x <- justMine
          xs <- buildRobotOptions
          return (x:xs)
        justMine = for2 memol0 go (timeLeft - 1) (income, M.unionWith (+) income stash)
        buildRobotOptions = do
          possibleRobots <- recPossibleRobots stash
          mapM (for2 memol0 go (timeLeft - 1) . second (M.unionWith (+) income) . buildRobots (income, stash)) possibleRobots
          where
            recPossibleRobots :: Stash -> MemoAll [[Robot]]
            recPossibleRobots stash =
              possibleRobots
                & mapM f
                & fmap concat
              where
                f robot = do
                  xss <- memol1 recPossibleRobots (buildRobot stash robot)
                  case xss of
                    [] -> return [[robot]]
                    xss -> return ((robot:) <$> xss)
                possibleRobots :: [Robot]
                possibleRobots = filter (canBuildRobot stash) bp.robots
        buildRobots :: (Income, Stash) -> [Robot] -> (Income, Stash)
        buildRobots (income, stash) robots = (newIncome, newStash)
          where
            newStash = foldl' buildRobot stash robots
            newIncome =
              robots
                <&> ((,1) . minedResource)
                & M.fromListWith (+)
                & M.unionWith (+) income
        buildRobot :: Stash -> Robot -> Stash
        buildRobot stash robot =
          robot.cost
            & foldl' (\stash (resource, cost) -> M.update (\x -> Just (x - cost)) resource stash) stash
        canBuildRobot :: Stash -> Robot -> Bool
        canBuildRobot stash robot =
          robot.cost
            & all (\(resource, cost) -> resourceCount resource stash >= cost)
    resourceCount :: Resource -> Map Resource Int -> Int
    resourceCount = M.findWithDefault 0
    geodeCount :: Map Resource Int -> Int
    geodeCount = resourceCount Geode


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
