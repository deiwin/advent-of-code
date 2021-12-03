module Practice.Y2019D02 (main) where

import Control.Applicative (empty)
import Control.Monad (foldM)
import Data.Function ((&))
import qualified Data.List as L
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import Data.Void (Void)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PL
import Prelude hiding (readFile)

type Parser = P.Parsec Void Text

parse :: Text -> Vector Int
parse input = VU.fromList $ run parser
  where
    parser = number `P.sepEndBy1` P.char ','
    number = lexeme PL.decimal
    lexeme = PL.lexeme spaceConsumer
    spaceConsumer :: Parser ()
    spaceConsumer = PL.space (P.skipSome (P.char ' ')) empty empty
    run p = case P.parse p "" input of
      Left bundle -> error (P.errorBundlePretty (bundle :: P.ParseErrorBundle Text Void))
      Right result -> result

solve1 :: Vector Int -> Int
solve1 program = runWithInputs program 12 2

solve2 :: Vector Int -> Int
solve2 program =
  options
    & fmap (uncurry (runWithInputs program))
    & zip options
    & L.find ((== 19690720) . snd)
    & maybe undefined fst
    & (\(noun, verb) -> 100 * noun + verb)
  where
    options = cartProd [0 .. 99] [0 .. 99]

runWithInputs :: Vector Int -> Int -> Int -> Int
runWithInputs program noun verb =
  runToCompletion modifiedProgram
    & (VU.! 0)
  where
    modifiedProgram = program VU.// [(1, noun), (2, verb)]

runToCompletion :: Vector Int -> Vector Int
runToCompletion program =
  foldM (\x _acc -> runStep x) (program, 0) [0 ..]
    & either id undefined

runStep :: (Vector Int, Int) -> Either (Vector Int) (Vector Int, Int)
runStep (state, pos)
  | state VU.! pos == 99 = Left state
  | state VU.! pos == 1 = Right (newState (+), newPos)
  | state VU.! pos == 2 = Right (newState (*), newPos)
  | otherwise = undefined
  where
    newState f = state VU.// [(valAt 3, f (state VU.! valAt 1) (state VU.! valAt 2))]
    valAt i = state VU.! (pos + i)
    newPos = pos + 4

cartProd :: [a] -> [a] -> [(a, a)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

main = do
  input <- readFile "Practice/inputs/Y2019D02.txt"
  runTestTT $
    TestCase $ do
      runToCompletion (parse "1,9,10,3,2,3,11,0,99,30,40,50")
        @?= VU.fromList [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
      runToCompletion (parse "1,0,0,0,99") @?= VU.fromList [2, 0, 0, 0, 99]
      runToCompletion (parse "2,3,0,3,99") @?= VU.fromList [2, 3, 0, 6, 99]
      runToCompletion (parse "2,4,4,5,99,0") @?= VU.fromList [2, 4, 4, 5, 99, 9801]
      runToCompletion (parse "1,1,1,4,99,5,6,0,99") @?= VU.fromList [30, 1, 1, 4, 2, 5, 6, 0, 99]
      solve1 (parse input) @?= 5290681
      solve2 (parse input) @?= 5741
