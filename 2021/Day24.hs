module Day24 (main) where

import Control.Applicative (empty, (<|>))
import Control.Arrow (first, second, (>>>))
import Control.Monad (foldM, guard, replicateM)
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
import Data.Tuple (swap)
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

data Var = W | X | Y | Z
  deriving (Eq, Show, Ord)

data Op
  = Inp Var
  | Add Var (Either Var Int)
  | Mul Var (Either Var Int)
  | Div Var (Either Var Int)
  | Mod Var (Either Var Int)
  | Eql Var (Either Var Int)
  deriving (Eq, Show, Ord)

type Program = [Op]

parse :: String -> Program
parse input = run (operation `P.endBy1` eol)
  where
    operation =
      inp
        <|> add
        <|> mul
        <|> div
        <|> mod
        <|> eql
    inp = Inp <$> (P.string "inp" *> spaces *> var)
    add = uncurry Add <$> twoArg "add"
    mul = uncurry Mul <$> twoArg "mul"
    div = uncurry Div <$> twoArg "div"
    mod = uncurry Mod <$> twoArg "mod"
    eql = uncurry Eql <$> twoArg "eql"
    var =
      W <$ P.char 'w'
        <|> X <$ P.char 'x'
        <|> Y <$ P.char 'y'
        <|> Z <$ P.char 'z'
    twoArg name = do
      P.string name *> spaces
      a <- var <* spaces
      b <- varOrInt
      return (a, b)
    varOrInt = Left <$> var <|> Right <$> number
    -- Standard parsers
    number :: P.ReadP Int
    number = do
      sign <- P.option "" ((: []) <$> P.char '-')
      digits <- P.munch1 C.isDigit
      return (read (sign ++ digits))
    spaces = P.many1 (P.char ' ')
    eol = P.char '\n'
    run p = fullMatch $ P.readP_to_S p input
    fullMatch :: [(a, [b])] -> a
    fullMatch = fst . fromJust . L.find (L.null . snd)

solve1 :: _
solve1 program = solveDijikstra f
  where
    f input = fst (eval program input) M.! Z

solveDijikstra :: ([Int] -> Int) -> _
solveDijikstra f = go IS.empty (MQ.singleton (toCost initialInput))
  where
    go :: IntSet -> MinQueue (Int, [Int]) -> [Int]
    -- go visited toVisit | traceShow (IS.size visited) False = undefined
    go visited toVisit =
      case MQ.minView toVisit of
        Nothing -> undefined
        Just ((cost, inputs), withoutVisit) ->
          let newToVisit =
                surrounding inputs
                  & filter ((`IS.notMember` visited) . toInt)
                  & fmap toCost
                  & MQ.fromList
                  & MQ.union withoutVisit
              newVisited = IS.insert (toInt inputs) visited
           in if IS.member (toInt inputs) visited
                then go visited withoutVisit
                else
                  if traceShow (cost, toInt inputs) cost == 0
                    then inputs
                    else go newVisited newToVisit

    surrounding :: [Int] -> [[Int]]
    surrounding inputs =
      (updateN inputs (\x -> x - 1) <$> [0 .. (length inputs - 1)])
        & (++ (updateN inputs (+ 1) <$> [0 .. (length inputs - 1)]))
        & filter (all (> 0))
        & filter (all (<= 9))
      where
        updateN inputs f n = snd . (\(i, x) -> if i == n then (i, f x) else (i, x)) <$> zip [0 ..] inputs
    toCost :: [Int] -> (Int, [Int])
    toCost inputs = (f inputs, inputs)
    -- initialInput = replicateM 14 [9,8..1]
    initialInput = replicate 14 9

toInt :: [Int] -> Int
toInt =
  reverse
    >>> zip ((10 ^) <$> [0 ..])
    >>> fmap (uncurry (*))
    >>> sum

toDigits :: Int -> [Int]
toDigits = reverse . L.unfoldr f
  where
    f 0 = Nothing
    f x = Just (swap (x `divMod` 10))

eval :: Program -> [Int] -> (Map Var Int, [Int])
eval = evalWith (M.fromList [(var, 0) | var <- [W, X, Y, Z]])

evalWith :: Map Var Int -> Program -> [Int] -> (Map Var Int, [Int])
evalWith initialState program input = foldl' go (initialState, input) program
  where
    go :: (Map Var Int, [Int]) -> Op -> (Map Var Int, [Int])
    go acc@(state, input) op =
      case op of
        Inp a -> case input of
          [] -> undefined
          (i : is) -> (M.insert a i state, is)
        Add a b -> twoArg acc a b (+)
        Mul a b -> twoArg acc a b (*)
        Div a b -> twoArg acc a b div
        Mod a b -> twoArg acc a b mod
        Eql a b -> twoArg acc a b (\a b -> if a == b then 1 else 0)
    twoArg (state, input) a (Left b) f = (M.insert a (f (state M.! a) (state M.! b)) state, input)
    twoArg (state, input) a (Right x) f = (M.insert a (f (state M.! a) x) state, input)

splitToInputBlocks :: Program -> [Program]
splitToInputBlocks = L.unfoldr go
  where
    go [] = Nothing
    go (x : xs) =
      L.break isInp xs
        & first (x :)
        & Just
    isInp (Inp _) = True
    isInp _ = False

evalProgramBlock :: Program -> Int -> Int -> Int -> Int
evalProgramBlock program n initialZ input =
  evalWith
    (M.fromList [(W, 0), (X, 0), (Y, 0), (Z, initialZ)])
    (splitToInputBlocks program !! n)
    [input]
    & fst
    & (M.! Z)

main = do
  input <- readFile "inputs/Day24.txt"
  exampleInput1 <- readFile "inputs/Day24_example1.txt"
  exampleInput2 <- readFile "inputs/Day24_example2.txt"
  -- print (take 2 (replicateM 14 [9,8..1]))
  -- print $ solve1 $ parse input
  runTestTT $
    TestCase $ do
      eval (parse exampleInput1) [11]
        @?= (M.fromList [(W, 1), (X, 0), (Y, 1), (Z, 1)], [])
      toDigits 1 @?= [1]
      toDigits 1234 @?= [1, 2, 3, 4]
      toDigits 13579246899999 @?= [1, 3, 5, 7, 9, 2, 4, 6, 8, 9, 9, 9, 9, 9]
      eval (parse input) (toDigits 13579246899999)
        @?= (M.fromList [(W, 9), (X, 1), (Y, 11), (Z, 112265567)], [])
      -- Largest
      -- block 0, add x 11, add y 8
      evalProgramBlock (parse input) 0 0 1 @?= 9 --           [1, ..] 1 + 8
      evalProgramBlock (parse input) 0 0 9 @?= 17 --          [9, ..] 9 + 8
      -- block 1, add x 12, add y 8
      evalProgramBlock (parse input) 1 9 1 @?= 243 --         [1, 1, ..] (1+8)*26 + (1 + 8)
      evalProgramBlock (parse input) 1 9 9 @?= 251 --         [1, 9, ..] (1+8)*26 + (9 + 8)
      evalProgramBlock (parse input) 1 17 9 @?= 459 --        [9, 9, ..] (9+8)*26 + (9 + 8)
      -- block 2, add x 10, add y 12
      evalProgramBlock (parse input) 2 243 1 @?= 6331 --      [1, 1, 1, ..] (1+8)*26^2 + (1+8)*26 + (1 + 12)
      evalProgramBlock (parse input) 2 243 5 @?= 6335 --      [1, 1, 5, ..] (1+8)*26^2 + (1+8)*26 + (5 + 12)
      evalProgramBlock (parse input) 2 243 9 @?= 6339 --      [1, 1, 9, ..] (1+8)*26^2 + (1+8)*26 + (9 + 12)
      evalProgramBlock (parse input) 2 251 1 @?= 6539 --      [1, 9, 1, ..] (1+8)*26^2 + (9+8)*26 + (1 + 12)
      evalProgramBlock (parse input) 2 459 1 @?= 11947 --     [9, 9, 1, ..] (9+8)*26^2 + (9+8)*26 + (1 + 12)
      evalProgramBlock (parse input) 2 459 5 @?= 11951 --     [9, 9, 5, ..] (9+8)*26^2 + (9+8)*26 + (9 + 12)
      evalProgramBlock (parse input) 2 459 9 @?= 11955 --     [9, 9, 9, ..] (9+8)*26^2 + (9+8)*26 + (9 + 12)
      -- block 3, add x -8, add y 10
      evalProgramBlock (parse input) 3 6327 1 @?= 243 --      Impossible
      evalProgramBlock (parse input) 3 6331 5 @?= 243 --      [1, 1, 1, 5, ..]
      evalProgramBlock (parse input) 3 6335 9 @?= 243 --      [1, 1, 5, 9, ..]
      evalProgramBlock (parse input) 3 11947 5 @?= 459 --     [9, 9, 1, 5, ..]
      evalProgramBlock (parse input) 3 11951 9 @?= 459 --     [9, 9, 5, 9, ..]
      -- block 4, add x 15, add y 2
      evalProgramBlock (parse input) 4 459 1 @?= 11937 --     [9, 9, 5, 9, 1, ..]
      evalProgramBlock (parse input) 4 459 9 @?= 11945 --     [9, 9, 5, 9, 9, ..]
      evalProgramBlock (parse input) 4 459 8 @?= 11944 --     [9, 9, 5, 9, 8, ..]
      -- block 5, add x 15, add y 8
      evalProgramBlock (parse input) 5 11945 1 @?= 310579 --  [9, 9, 5, 9, 9, 1, ..]
      evalProgramBlock (parse input) 5 11945 9 @?= 310587 --  [9, 9, 5, 9, 9, 9, ..]
      evalProgramBlock (parse input) 5 11944 9 @?= 310561 --  [9, 9, 5, 9, 8, 9, ..]
      -- block 6, add x -11, add y 4
      evalProgramBlock (parse input) 6 310587 6 @?= 11945 --  [9, 9, 5, 9, 9, 9, 6, ..]
      evalProgramBlock (parse input) 6 310561 6 @?= 11944 --  [9, 9, 5, 9, 8, 9, 6, ..]
      -- block 7, add x 10, add y 9
      evalProgramBlock (parse input) 7 11945 1 @?= 310580 --  [9, 9, 5, 9, 9, 9, 6, 1, ..]
      evalProgramBlock (parse input) 7 11945 3 @?= 310582 --  [9, 9, 5, 9, 9, 9, 6, 3, ..]
      evalProgramBlock (parse input) 7 11945 9 @?= 310588 --  [9, 9, 5, 9, 9, 9, 6, 9, ..]
      evalProgramBlock (parse input) 7 11944 1 @?= 310554 --  [9, 9, 5, 9, 8, 9, 6, 1, ..]
      evalProgramBlock (parse input) 7 11944 3 @?= 310556 --  [9, 9, 5, 9, 8, 9, 6, 1, ..]
      evalProgramBlock (parse input) 7 11944 9 @?= 310562 --  [9, 9, 5, 9, 8, 9, 6, 9, ..]
      -- block 8, add x -3, add y 10
      evalProgramBlock (parse input) 8 310580 7 @?= 11945 --  [9, 9, 5, 9, 9, 9, 6, 1, 7, ..]
      evalProgramBlock (parse input) 8 310582 9 @?= 11945 --  [9, 9, 5, 9, 9, 9, 6, 3, 9, ..]
      evalProgramBlock (parse input) 8 310554 7 @?= 11944 --  [9, 9, 5, 9, 8, 9, 6, 1, 7, ..]
      evalProgramBlock (parse input) 8 310556 9 @?= 11944 --  [9, 9, 5, 9, 8, 9, 6, 3, 9, ..]
      -- block 9, add x 15, add y 3
      evalProgramBlock (parse input) 9 11945 1 @?= 310574 --  [9, 9, 5, 9, 9, 9, 6, 1, 9, 1, ..]
      evalProgramBlock (parse input) 9 11945 9 @?= 310582 --  [9, 9, 5, 9, 9, 9, 6, 1, 9, 9, ..]
      evalProgramBlock (parse input) 9 11944 1 @?= 310548 --  [9, 9, 5, 9, 8, 9, 6, 3, 9, 1, ..]
      evalProgramBlock (parse input) 9 11944 9 @?= 310556 --  [9, 9, 5, 9, 8, 9, 6, 3, 9, 9, ..]
      -- block 10, add x -3, add y 7
      evalProgramBlock (parse input) 10 310548 1 @?= 11944 -- [9, 9, 5, 9, 8, 9, 6, 3, 9, 1, 1, ..]
      evalProgramBlock (parse input) 10 310556 9 @?= 11944 -- [9, 9, 5, 9, 8, 9, 6, 3, 9, 9, 9, ..]
      -- block 11, add x -1, add y 7
      evalProgramBlock (parse input) 11 11944 9 @?= 459 --    [9, 9, 5, 9, 8, 9, 6, 3, 9, 9, 9, 9, ..]
      -- block 12, add x -10, add y 2
      evalProgramBlock (parse input) 12 459 7 @?= 17 --       [9, 9, 5, 9, 8, 9, 6, 3, 9, 9, 9, 9, 7, ..]
      -- block 13, add x -16, add y 2
      evalProgramBlock (parse input) 13 17 1 @?= 0 --         [9, 9, 5, 9, 8, 9, 6, 3, 9, 9, 9, 9, 7, 1]
      fst (eval (parse input) (toDigits 99598963999971)) M.! Z @?= 0

      -- Smallest
      -- block 0, add x 11, add y 8
      evalProgramBlock (parse input) 0 0 9 @?= 17 --          [9, ..] 1 + 8
      -- block 1, add x 12, add y 8
      evalProgramBlock (parse input) 1 17 3 @?= 453 --        [9, 3, ..] (1+8)*26 + (1 + 8)
      -- block 2, add x 10, add y 12
      evalProgramBlock (parse input) 2 453 1 @?= 11791 --     [9, 3, 1, ..] (1+8)*26^2 + (1+8)*26 + (1 + 12)
      -- block 3, add x -8, add y 10
      evalProgramBlock (parse input) 3 11791 5 @?= 453 --     [9, 3, 1, 5, ..]
      -- block 4, add x 15, add y 2
      evalProgramBlock (parse input) 4 453 1 @?= 11781 --     [9, 3, 1, 5, 1, ..]
      -- block 5, add x 15, add y 8
      evalProgramBlock (parse input) 5 11781 4 @?= 306318 --  [9, 3, 1, 5, 1, 4, ..]
      -- block 6, add x -11, add y 4
      evalProgramBlock (parse input) 6 306318 1 @?= 11781  -- [9, 3, 1, 5, 1, 4, 1, ..]
      -- block 7, add x 10, add y 9
      evalProgramBlock (parse input) 7 11781 1 @?= 306316 --  [9, 3, 1, 5, 1, 4, 1, 1, ..]
      -- block 8, add x -3, add y 10
      evalProgramBlock (parse input) 8 306316 7 @?= 11781 --  [9, 3, 1, 5, 1, 4, 1, 1, 7, ..]
      -- block 9, add x 15, add y 3
      evalProgramBlock (parse input) 9 11781 1 @?= 306310 --  [9, 3, 1, 5, 1, 4, 1, 1, 7, 1, ..]
      -- block 10, add x -3, add y 7
      evalProgramBlock (parse input) 10 306310 1 @?= 11781 -- [9, 3, 1, 5, 1, 4, 1, 1, 7, 1, 1, ..]
      -- block 11, add x -1, add y 7
      evalProgramBlock (parse input) 11 11781 2 @?= 453 --    [9, 3, 1, 5, 1, 4, 1, 1, 7, 1, 1, 2, ..]
      -- block 12, add x -10, add y 2
      evalProgramBlock (parse input) 12 453 1 @?= 17 --       [9, 3, 1, 5, 1, 4, 1, 1, 7, 1, 1, 2, 1, ..]
      -- block 13, add x -16, add y 2
      evalProgramBlock (parse input) 13 17 1 @?= 0 --         [9, 3, 1, 5, 1, 4, 1, 1, 7, 1, 1, 2, 1, 1]
      fst (eval (parse input) (toDigits 93151411711211)) M.! Z @?= 0
