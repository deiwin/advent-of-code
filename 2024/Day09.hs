module Day09 (main) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.Maybe (isJust)
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)

parse :: String -> [Char]
parse = head . lines

readInput :: [Char] -> [(Int, Maybe Int)]
readInput input =
  zip (read . (: []) <$> input) fileIDs
  where
    fileIDs = L.intersperse Nothing (Just <$> [0 ..])

solve1 :: [Char] -> Int
solve1 input =
  f files spaces (reverse files)
    & zipWith (*) [0 ..]
    & sum
  where
    maxSize = sum $ fst <$> files
    (files', spaces') = L.partition (isJust . snd) $ readInput input
    files :: [(Int, Int)] -- (fileSize, fileID)
    files = (\(x, Just y) -> (x, y)) <$> files'
    spaces :: [Int] -- spaceSize
    spaces = fst <$> spaces'
    init = ([], 0)
    f :: [(Int, Int)] -> [Int] -> [(Int, Int)] -> [Int]
    f fs ss efs = concat $ reverse $ fst $ f' init fs ss efs
    f' :: ([[Int]], Int) -> [(Int, Int)] -> [Int] -> [(Int, Int)] -> ([[Int]], Int)
    f' (res, filledSize) ((fSize, fID) : fs) (spaceSize : ss) ((efSize, efID) : efs)
      | newFilledSize >= maxSize = (newRes, maxSize)
      | efSize == spaceSize = f' (newRes, newFilledSize) fs ss efs
      | efSize > spaceSize = f' (newRes, newFilledSize) fs ss ((efSize - spaceSize, efID) : efs)
      | efSize < spaceSize = f' (newRes, newFilledSize) (emptyF : fs) (spaceSize - efSize : ss) efs
      where
        newRes = take (maxSize - filledSize) (fChunk ++ efChunk) : res
        fChunk = replicate fSize fID
        emptyF = (0, fID)
        efChunk = take spaceSize $ replicate efSize efID
        newFilledSize = filledSize + fSize + min spaceSize efSize
    f' _ _ _ _ = undefined

solve2 :: [Char] -> Int
solve2 input =
  reverse files
    & L.mapAccumL f spaces
    & snd
    <&> checksum
    & sum
  where
    (files', spaces') =
      readInput input
        & L.scanl' (\(i, (oldS, _)) (s, f) -> (i + oldS, (s, f))) (0, (0, Nothing))
        & tail
        & L.partition (isJust . snd . snd)
    files :: [(Int, Int, Int)] -- (start, fileSize, fileID)
    files = (\(start, (x, Just y)) -> (start, x, y)) <$> files'
    spaces :: [(Int, Int)] -- (start, spaceSize)
    spaces = (\(start, (x, Nothing)) -> (start, x)) <$> spaces'
    checksum :: (Int, Int, Int) -> Int
    checksum (start, fSize, fID) =
      start
        & iterate (+ 1)
        & take fSize
        <&> (* fID)
        & sum
    f :: [(Int, Int)] -> (Int, Int, Int) -> ([(Int, Int)], (Int, Int, Int))
    f spaces file@(start, fSize, fID) = (newSpaces, newFile)
      where
        newSpaces =
          case fittingSpaceIndexM of
            Just fittingSpaceIndex ->
              let (init, (sStart, sSize) : rest) = splitAt fittingSpaceIndex spaces
               in if sSize == fSize
                    then init ++ rest
                    else init ++ ((sStart + fSize, sSize - fSize) : rest)
            Nothing -> spaces
        newFile =
          case fittingSpaceIndexM of
            Just fittingSpaceIndex ->
              let (sStart, _sSize) = spaces !! fittingSpaceIndex
               in (sStart, fSize, fID)
            Nothing -> file
        fittingSpaceIndexM =
          spaces
            & takeWhile (\(sStart, _sSize) -> sStart < start)
            & L.findIndex (\(_sStart, sSize) -> sSize >= fSize)

main = do
  input <- readFile "inputs/Day09.txt"
  exampleInput <- readFile "inputs/Day09_example.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse exampleInput) @?= 1928
      solve1 (parse input) @?= 6283404590840
      solve2 (parse exampleInput) @?= 2858
      solve2 (parse input) @?= 6304576012713
