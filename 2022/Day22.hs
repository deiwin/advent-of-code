module Day22 (main) where

import Control.Applicative ((<|>))
import Control.Arrow (first)
import Data.Char qualified as C
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl')
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, mapMaybe)
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import Text.ParserCombinators.ReadP qualified as P
import Prelude hiding (Either (..))

type Coord = V2 Int

data Cell = Open | Wall
  deriving (Eq, Show, Ord)

data Input = Input
  { map :: Map Coord Cell,
    commands :: [Command]
  }
  deriving (Eq, Show, Ord)

data Command = Step Int | Turn TurnDirection
  deriving (Eq, Show, Ord)

data Direction = Right | Down | Left | Up
  deriving (Eq, Show, Ord, Enum, Bounded)

data TurnDirection = TurnLeft | TurnRight
  deriving (Eq, Show, Ord)

parse :: String -> Input
parse input = run inputP
  where
    inputP =
      Input
        <$> (map <* P.count 2 eol)
        <*> (commands <* eol)
    map =
      mapLine
        `P.sepBy` eol
        <&> (M.fromList . concatMap (\(y, cells) -> first (V2 y) <$> cells) . zip [1 ..])
      where
        mapLine :: P.ReadP [(Int, Cell)]
        mapLine =
          P.many1 maybeCell
            <&> (mapMaybe (\(x, m) -> (x,) <$> m) . zip [1 ..])
        maybeCell =
          Nothing <$ P.char ' '
            <|> Just Open <$ P.char '.'
            <|> Just Wall <$ P.char '#'
    commands = P.many1 command
    command =
      Step <$> number
        <|> Turn <$> turnDirection
    turnDirection =
      TurnLeft <$ P.char 'L'
        <|> TurnRight <$ P.char 'R'
    -- Standard parsers
    number :: P.ReadP Int
    number = read <$> P.munch1 C.isDigit
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

data State = State
  { location :: Coord,
    direction :: Direction
  }
  deriving (Eq, Show)

initialState :: Input -> State
initialState input = State {..}
  where
    location = fst $ M.findMin input.map
    direction = Right

succ' :: (Eq a, Enum a, Bounded a) => a -> a
succ' x
  | maxBound == x = minBound
  | otherwise = succ x

pred' :: (Eq a, Enum a, Bounded a) => a -> a
pred' x
  | minBound == x = maxBound
  | otherwise = pred x

move :: Input -> (Input -> State -> State) -> State -> Command -> State
move input wrapF s = \case
  (Turn TurnLeft) -> s {direction = pred' s.direction}
  (Turn TurnRight) -> s {direction = succ' s.direction}
  (Step 0) -> s
  (Step n) ->
    case input.map M.! newState.location of
      Wall -> s
      Open -> move input wrapF newState (Step (n - 1))
    where
      newState = maybe (wrapF input s) (locToState . const naiveNewLocation) (input.map M.!? naiveNewLocation)
      locToState c = s {location = c}
      naiveNewLocation :: Coord
      naiveNewLocation = s.location + moveDiff s.direction

moveDiff :: Direction -> V2 Int
moveDiff = \case
  Right -> V2 0 1
  Down -> V2 1 0
  Left -> V2 0 (-1)
  Up -> V2 (-1) 0

score :: State -> Int
score s =
  (1000 * getY s.location)
    + (4 * getX s.location)
    + fromEnum s.direction

getY (V2 y _) = y

getX (V2 _ x) = x

wrapToOtherSide :: Input -> State -> State
wrapToOtherSide input s =
  moveDiff otherWay
    & iterate (+ moveDiff otherWay)
    <&> (+ s.location)
    & takeWhile (`M.member` input.map)
    & last
    & (\c -> s {location = c})
  where
    otherWay =
      case s.direction of
        Right -> Left
        Down -> Up
        Left -> Right
        Up -> Down

solve1 :: Input -> Int
solve1 input =
  foldl' (move input wrapToOtherSide) (initialState input) input.commands
    & score

wrapAroundCube :: Input -> State -> State
wrapAroundCube _ s
  | cond Up (1, 1) (51, 100) =
      State {direction = Right, location = V2 (150 + mod' x) 1}
  | cond Up (1, 1) (101, 150) =
      State {direction = Up, location = V2 200 (mod' x)}
  | cond Right (1, 50) (150, 150) =
      State {direction = Left, location = V2 (100 + imod y) 100}
  | cond Down (50, 50) (101, 150) =
      State {direction = Left, location = V2 (50 + mod' x) 100}
  | cond Right (51, 100) (100, 100) =
      State {direction = Up, location = V2 50 (100 + mod' y)}
  | cond Right (101, 150) (100, 100) =
      State {direction = Left, location = V2 (imod y) 150}
  | cond Down (150, 150) (51, 100) =
      State {direction = Left, location = V2 (150 + mod' x) 50}
  | cond Right (151, 200) (50, 50) =
      State {direction = Up, location = V2 150 (50 + mod' y)}
  | cond Down (200, 200) (1, 50) =
      State {direction = Down, location = V2 1 (100 + mod' x)}
  | cond Left (151, 200) (1, 1) =
      State {direction = Down, location = V2 1 (50 + mod' y)}
  | cond Left (101, 150) (1, 1) =
      State {direction = Right, location = V2 (imod y) 51}
  | cond Up (101, 101) (1, 50) =
      State {direction = Right, location = V2 (50 + mod' x) 51}
  | cond Left (51, 100) (51, 51) =
      State {direction = Down, location = V2 101 (mod' y)}
  | cond Left (1, 50) (51, 51) =
      State {direction = Right, location = V2 (100 + imod y) 1}
  | otherwise = undefined
  where
    x = getX s.location
    y = getY s.location
    imod x = 51 - mod' x
    mod' x = ((x - 1) `mod` 50) + 1
    cond :: Direction -> (Int, Int) -> (Int, Int) -> Bool
    cond dir (minY, maxY) (minX, maxX) =
      s.direction == dir
        && y >= minY
        && y <= maxY
        && x >= minX
        && x <= maxX

solve2 :: Input -> Int
solve2 input =
  foldl' (move input wrapAroundCube) (initialState input) input.commands
    & score

main = do
  input <- readFile "inputs/Day22.txt"
  exampleInput <- readFile "inputs/Day22_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 6032
      solve1 (parse input) @?= 3590
      solve2 (parse input) @?= 86382
