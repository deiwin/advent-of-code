module Day23 (main) where

import Control.Applicative ((<|>))
import Control.Arrow (second)
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import qualified Data.Char as C
import Data.Function ((&))
import Data.Ix (inRange, range)
import qualified Data.List as L
import Data.Maybe (fromJust, mapMaybe)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MQ
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace (trace, traceShow)
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.ParserCombinators.ReadP as P

data Amphiboid = A | B | C | D
  deriving (Eq, Read, Show, Ord)

data Cell = Wall | Open | Amph Amphiboid
  deriving (Eq, Show, Ord)

type Grid = Array (V2 Int) Cell

data Move = Move
  { from :: V2 Int,
    to :: V2 Int,
    amph :: Amphiboid
  }
  deriving (Eq, Show, Ord)

type Visit = (Int, Int, Int, [Move], Grid)

parse :: String -> [[Cell]]
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

solve1 :: [[Cell]] -> Int
solve1 input = solveDijikstra (buildGrid input)

solve2 :: [[Cell]] -> Int
solve2 input = solveDijikstra (buildGrid modifiedInput)
  where
    modifiedInput = before ++ addition ++ after
    addition =
      [ [Wall, Wall, Wall, Amph D, Wall, Amph C, Wall, Amph B, Wall, Amph A, Wall],
        [Wall, Wall, Wall, Amph D, Wall, Amph B, Wall, Amph A, Wall, Amph C, Wall]
      ]
    (before, after) = L.splitAt 3 input

solveDijikstra :: Grid -> Int
solveDijikstra grid = go (0, 0, 0, [], grid) S.empty MQ.empty
  where
    go :: Visit -> Set Grid -> MinQueue Visit -> Int
    go (_heuristicCost, cost, i, previousMoves, grid) visited toVisit
      | isFinished grid = cost
      | grid `S.member` visited =
        case MQ.minView toVisit of
          Nothing -> error "a"
          Just (visit, newToVisit) -> go visit visited newToVisit
      | traceShow (i, cost) False = undefined
      | trace (showGrid grid) False = undefined
      | otherwise =
        case MQ.minView newToConsiderToVisit of
          Nothing -> error "b"
          Just (visit, newToVisit) -> go visit newVisited newToVisit
      where
        newVisited = S.insert grid visited
        newToConsiderToVisit =
          possibleMoves previousMoves grid
            & fmap forQueue
            & filter (\(_, _, _, _, grid) -> grid `S.notMember` newVisited)
            & MQ.fromList
            & MQ.union toVisit
        forQueue :: Move -> Visit
        forQueue move =
          ( cost + moveCost move + heuristicCost (update grid move),
            cost + moveCost move,
            i + 1,
            move : previousMoves,
            update grid move
          )

update :: Grid -> Move -> Grid
update grid move = grid A.// [(from move, Open), (to move, Amph (amph move))]

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
              [V2 y 0 | y <- [1 .. 4]]
                & fmap (+ c)
                & filter (inRange (A.bounds grid))
                & any ((`notElem` [Open, Wall, Amph amph]) . (grid A.!))
        getY (V2 y _) = y
        getX (V2 _ x) = x
        expectedX amph = getX (head (destinations amph))

possibleMoves :: [Move] -> Grid -> [Move]
possibleMoves allPreviousMoves grid =
  amphiboidLocations grid
    & concatMap (\(pos, amph) -> toMove amph pos <$> surrounding pos)
    & filter isLegalMove
  where
    toMove amph from to = Move {..}
    isLegalMove :: Move -> Bool
    isLegalMove move
      | not (movesToAnOpenCell move) = False
      | stoppingInNoStopZone move = False
      | movingIntoWrongRoom move = False
      | movingIntoCorrectRoom move && destinationRoomOccupiedByWrongAmphiboids move = False
      | otherMoveFromCorridorToRoomInProgress move = False
      | otherwise = True
    movesToAnOpenCell move = (grid A.! to move) == Open
    stoppingInNoStopZone move =
      case allPreviousMoves of
        [] -> False
        (previousMove : _) ->
          to previousMove `elem` noStop
            && not (movingSameAmphiboid previousMove move)
    movingSameAmphiboid previousMove move = to previousMove == from move
    movingIntoWrongRoom move =
      from move `elem` corridor
        && to move `elem` wrongRoomCoords (amph move)
    wrongRoomCoords amph =
      [A, B, C, D]
        & filter (/= amph)
        & concatMap destinations
    movingIntoCorrectRoom move =
      from move `elem` corridor
        && to move `elem` destinations (amph move)
    destinationRoomOccupiedByWrongAmphiboids move =
      amph move
        & destinations
        & filter (inRange (A.bounds grid))
        & any ((`notElem` [Wall, Open, Amph (amph move)]) . (grid A.!))
    otherMoveFromCorridorToRoomInProgress move =
      case consecutivePreviousMoves of
        [] -> False
        consPrevMoves@(previousMove : _) ->
          from (last consPrevMoves) `elem` corridor
            && to previousMove `elem` corridor
            && not (movingSameAmphiboid previousMove move)
    consecutivePreviousMoves =
      case allPreviousMoves of
        [] -> []
        (previousMove : rest) ->
          zip rest allPreviousMoves
            & filter (uncurry movingSameAmphiboid)
            & fmap fst
            & (previousMove :)

surrounding :: V2 Int -> [V2 Int]
surrounding c = (+ c) <$> [V2 0 (-1), V2 0 1, V2 (-1) 0, V2 1 0]

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

moveCost :: Move -> Int
moveCost = amphStepCost . amph

destinations :: Amphiboid -> [V2 Int]
destinations A = [V2 y 3 | y <- [2 .. 5]]
destinations B = [V2 y 5 | y <- [2 .. 5]]
destinations C = [V2 y 7 | y <- [2 .. 5]]
destinations D = [V2 y 9 | y <- [2 .. 5]]

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
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 12521
      solve1 (parse input) @?= 16244
      solve2 (parse exampleInput) @?= 43136
      solve2 (parse input) @?= 43226 -- Solved manually really :/ Takes too long to run.
