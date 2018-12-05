#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc
{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldr)
import Data.Char (toLower, isLower)

main :: IO ()
main = do
    input <- head . lines <$> readFile "day5.input"
    print $ length $ untilFixed reduce input

untilFixed :: Eq a => (a -> a) -> a -> a
untilFixed f x
  | newX == x = x
  | otherwise = untilFixed f newX
  where
    newX = f x

reduce :: String -> String
reduce = foldr reduce' []
reduce' :: Char -> String -> String
reduce' char [] = [char]
reduce' char (headChar : rest)
  | reducable char headChar = rest
  | otherwise               = char : headChar : rest

reducable :: Char -> Char -> Bool
reducable a b = sameChar && differentCase
  where
    sameChar      = toLower a == toLower b
    differentCase = isLower a /= isLower b
