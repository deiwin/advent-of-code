module Day19 (main) where

import Data.Tuple (swap)
import Control.Applicative (empty, (<|>), liftA2)
import Control.Arrow (first, second, (>>>))
import Data.Bifunctor (bimap)
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

type Memo0 = MemoT (Int, Income) (Income, Stash)
type Memo1 = MemoT Stash (Set (Income, Stash))
type Memo = Memo0 (Memo1 Identity)

maxGeodes :: Blueprint -> _
-- maxGeodes bp = geodeCount $ go 24 (M.singleton Ore 1) M.empty
maxGeodes bp = startEvalMemo $ startEvalMemoT $ go 12 (M.singleton Ore 1)
  where
    go :: Int -> Income -> Memo (Income, Stash)
    go timeLeft income
      -- | traceShow (timeLeft, income) False = undefined
      | timeLeft == 0 = return (income, M.empty)
      -- | timeLeft == 1 = return (income, income)
      -- | otherwise = traceShowId <$> bestOption
      | otherwise = bestOption
      -- | otherwise = L.maximumBy (comparing (geodeCount . snd)) <$> options
      -- | otherwise = (,) <$> newIncome <*> newStash
      where
        -- prev = for2 memol0 go (timeLeft - 1) income
        -- newStash = uncurry (M.unionWith (+)) <$> prev
        -- newIncome :: Memo Income
        -- newIncome = L.maximumBy (comparing geodeCount) <$> incomeOptions
        -- incomeOptions :: Memo [Income]
        -- incomeOptions = undefined
        bestOption :: Memo (Income, Stash)
        bestOption = L.maximumBy (comparing f) <$> options
          where f = bimap geodeCount geodeCount . swap
        options :: Memo [(Income, Stash)]
        options = justMine <:> buildRobotOptions
          where a <:> b = liftA2 (:) a b
        justMine :: Memo (Income, Stash)
        justMine =
          for2 memol0 go (timeLeft - 1) income
            <&> (\(income, stash) -> (income, M.unionWith (+) income stash))
        buildRobotOptions :: Memo [(Income, Stash)]
        buildRobotOptions = do
          (newIncome, newStash) <- for2 memol0 go (timeLeft - 1) income
          options <- buildOptionsForStash newStash
          options
            & S.toList
            <&> first (M.unionWith (+) newIncome)
            -- <&> bimap (M.unionWith (+) newIncome) (M.unionWith (+) newIncome)
            & return
          where
            buildOptionsForStash :: Stash -> Memo (Set (Income, Stash))
            buildOptionsForStash stash =
              possibleRobots
                & mapM (f . (\r -> traceShow (r, stash) r))
                <&> foldl' S.union S.empty
              where
                f :: Robot -> Memo (Set (Income, Stash))
                f robot = do
                  xss <- memol1 buildOptionsForStash newStash
                  if S.null xss
                     then return $ S.singleton (newIncome, newStash)
                     else return $ S.map (first (M.unionWith (+) newIncome)) xss
                  where
                    newStash = buildRobot stash robot
                    newIncome = M.singleton robot.minedResource 1
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
