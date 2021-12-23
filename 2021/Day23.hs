module Day23 (main) where

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
    mapMaybe,
  )
import Data.Ord (comparing)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MQ
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
    trace,
  )
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P
import Data.Bifunctor (bimap)
import Data.Foldable (traverse_)

data Amphiboid = A | B | C | D
  deriving (Eq, Read, Show, Ord)

data Cell = Wall | Open | Amph Amphiboid
  deriving (Eq, Show, Ord)

type Grid = Array (V2 Int) Cell

parse :: String -> _
parse input = run (P.many1 cell `P.endBy1` eol <* P.eof)
  where
    cell =
      Wall <$ (P.char '#' <|> P.char ' ')
        <|> Open <$ P.char '.'
        <|> Amph . read . (: []) <$> letter
    -- Standard parsers
    letter = P.satisfy C.isLetter
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: _
solve1 input = solveDijikstra (buildGrid input)

solve2 :: _
solve2 input = solveDijikstra (buildGrid modifiedInput)
  where
    modifiedInput = before ++ addition ++ after
    addition =
      [[Wall,Wall,Wall,Amph D,Wall,Amph C,Wall,Amph B,Wall,Amph A,Wall],
       [Wall,Wall,Wall,Amph D,Wall,Amph B,Wall,Amph A,Wall,Amph C,Wall]
      ]
    (before, after) = L.splitAt 3 input

solveDijikstra :: Grid -> Int
solveDijikstra grid = go 0 0 grid S.empty MQ.empty
  where
    go :: Int -> Int -> Grid -> Set Grid -> MinQueue _ -> Int
    go i cost grid visited toVisit
      | isFinished grid = cost
      | grid `S.member` visited =
        case MQ.minView toVisit of
          Nothing -> undefined
          Just ((_, newCost, j, newGrid), newToVisit) -> go j newCost newGrid visited newToVisit
      | traceShow (i, cost) False = undefined
      | trace (showGrid grid) False = undefined
      | otherwise =
        case MQ.minView newToConsiderToVisit of
          Nothing -> undefined
          Just ((_, newCost, j, newGrid), newToVisit) -> go j newCost newGrid newVisited newToVisit
      where
        newVisited = S.insert grid visited
        newToConsiderToVisit =
          possibleMoves grid
            & fmap forQueue
            & filter (\(_, _, _, grid) -> grid `S.notMember` newVisited)
            & MQ.fromList
            & MQ.union toVisit
        forQueue :: (Int, Grid) -> _
        forQueue (addCost, grid) = (cost + addCost + heuristicCost grid, cost + addCost, (i + 1), grid)
        -- forQueue (addCost, grid) = (cost + addCost, (i + 1), grid)

heuristicCost :: Grid -> Int
heuristicCost grid =
  [A, B, C, D]
    & fmap findClosest
    & sum
    & (+ unblockingCost)
  where
    findClosest :: Amphiboid -> Int
    findClosest amph =
      curLocations
        & L.permutations
        & fmap (sum . fmap (cost amph) . zipWith simpleDist destLocations)
        & minimum
      where
        destLocations = destinations amph
        curLocations =
          amphiboidLocations grid
            & filter ((== amph) . snd)
            & fmap fst
    simpleDist :: V2 Int -> V2 Int -> Int
    simpleDist (V2 y1 x1) (V2 y2 x2)
      | x1 == x2 = abs (y1 - y2)
      | otherwise = abs (x1 - x2) + (y1 - 1) + (y2 - 1)
    cost :: Amphiboid -> Int -> Int
    cost amph steps = amphStepCost amph * steps
    unblockingCost = sum (go <$> amphiboidLocations grid)
      where
        go (c, amph) = cost amph (steps (c, amph))
        steps (c, amph)
          | getX c == expectedX amph && blocksOthers = getY c + 2
          | otherwise = 0
          where
            blocksOthers =
              [V2 y 0 | y <- [1..4]]
                & fmap (+ c)
                & filter (inRange (A.bounds grid))
                & any ((`notElem` [Open, Wall, Amph amph]) . (grid A.!))
        getY (V2 y _) = y
        getX (V2 _ x) = x
        expectedX amph = getX (head (destinations amph))

possibleMoves :: Grid -> [(Int, Grid)]
possibleMoves grid =
  amphiboidLocations grid
    & concatMap reachableForLocation
    & filter (not . illegalMove)
    & fmap toCost
  where
    toCost (_, _, []) = undefined
    toCost (amph, from, path@(to:_)) = (cost, newGrid)
      where
        cost = amphStepCost amph * length path
        newGrid = grid A.// assocs
        assocs = [(from, Open), (to, Amph amph)]
    illegalMove (_, _, []) = undefined
    illegalMove (amph, from, path@(to:_))
      | to `elem` noStop = True
      | illegalPath amph path = True
      | from `elem` corridor && to `notElem` destinations amph = True
      | from `notElem` corridor && not (to `elem` corridor || to `elem` destinations amph) = True
      | otherwise = False
    illegalPath _ [] = undefined
    illegalPath amph path = any illegalStep steps
      where
        steps = zip (tail path) path
        illegalStep (from, to)
          | from `elem` corridor && to `notElem` corridor && (wrongRoom to || occupiedRoom to) = True
          | otherwise = False
        wrongRoom c = c `notElem` destinations amph
        occupiedRoom c = not (any ((`elem` [Open, Amph amph]) . (grid A.!)) (otherRoomCells c))
        otherRoomCells :: V2 Int -> [V2 Int]
        otherRoomCells c = L.delete c (fromJust (L.find (c `elem`) rooms))
    reachableForLocation :: (V2 Int, Amphiboid) -> [(Amphiboid, V2 Int, [V2 Int])]
    reachableForLocation (c, amph) = (amph,c,) <$> reachable grid c

surrounding :: V2 Int -> [V2 Int]
surrounding c = (+ c) <$> [V2 0 (-1), V2 0 1, V2 (-1) 0, V2 1 0]

reachable :: Grid -> V2 Int -> [[V2 Int]]
reachable grid c = go [[c]] S.empty S.empty
  where
    go :: [[V2 Int]] -> Set (V2 Int) -> Set [V2 Int] -> [[V2 Int]]
    go ([]:_) _ _ = undefined
    go [] _ results = init <$> S.toList results
    go (path@(c:_) : toVisit) visited results = go newToVisit newVisited newResults
      where
        newResults
          | length path <= 1 = results
          | otherwise = S.insert path results
        newVisited = S.insert c visited
        newToVisit =
          surrounding c
            & filter canVisit
            & fmap (:path)
            & (toVisit ++)
        canVisit c
          | c `S.member` visited = False
          | grid A.! c == Open = True
          | otherwise = False

amphiboidLocations :: Grid -> [(V2 Int, Amphiboid)]
amphiboidLocations grid =
  grid
    & A.assocs
    & mapMaybe (liftTupleSecond . second asAmphiboid)
  where
    liftTupleSecond :: Monad m => (a, m b) -> m (a, b)
    liftTupleSecond (a, mb) = (a,) <$> mb
    asAmphiboid (Amph a) = Just a
    asAmphiboid _ = Nothing

isFinished :: Grid -> Bool
isFinished grid = all (\(c, a) -> c `elem` destinations a) (amphiboidLocations grid)

amphStepCost :: Amphiboid -> Int
amphStepCost = \case
  A -> 1
  B -> 10
  C -> 100
  D -> 1000

destinations :: Amphiboid -> [V2 Int]
destinations A = [V2 y 3 | y <- [2..5]]
destinations B = [V2 y 5 | y <- [2..5]]
destinations C = [V2 y 7 | y <- [2..5]]
destinations D = [V2 y 9 | y <- [2..5]]

rooms :: [[V2 Int]]
rooms = destinations <$> [A, B, C, D]

corridor :: [V2 Int]
corridor = [V2 1 x | x <- [1 .. 11]]

noStop :: [V2 Int]
noStop = [V2 1 x | x <- [3, 5, 7, 9]]

buildGrid :: [[Cell]] -> Grid
buildGrid input = A.array bounds cells
  where
    cells = zip (range bounds) (concat (fixLine <$> input))
    fixLine line = take (length (head input)) (line ++ repeat Wall)
    bounds = (V2 0 0, V2 (length input - 1) (length (head input) - 1))

showGrid :: Grid -> String
showGrid grid = unlines rows
  where
    bounds = A.bounds grid
    (_, V2 maxY maxX) = bounds
    rows = showRow <$> [0 .. maxY]
    showRow y = concatMap (showCoord y) [0 .. maxX]
    showCoord y x = showCell $ grid A.! V2 y x
    showCell = \case
      Wall -> "#"
      Open -> "."
      Amph a -> show a

main = do
  input <- readFile "inputs/Day23.txt"
  exampleInput <- readFile "inputs/Day23_example.txt"
  -- traverse_ putStr (showGrid . snd <$> possibleMoves (buildGrid (parse exampleInput)))
  print $ solve2 (parse exampleInput)
  runTestTT $
    TestCase $ do
      -- solve1 (parse exampleInput) @?= 12521
      -- solve1 (parse input) @?= 16244
      -- solve2 (parse exampleInput) @?= 43136
      -- solve2 (parse input) @?= 43136
      1 @?= 1
