{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day1 (histogram, parseLists, parseNumbers, part1, part2, run) where

import Data.List (sort, transpose)
import Data.List.Split (dropBlanks, dropDelims, oneOf, split)
import Data.Map (Map, empty, findWithDefault, insertWith)

parseNumbers :: String -> [Int]
parseNumbers s = map read $ split (dropBlanks . dropDelims $ oneOf " ") s

parseLists :: String -> [[Int]]
parseLists s = transpose $ map parseNumbers $ lines s

histogram :: [Int] -> Map Int Int
histogram ns = foldr (\n m -> insertWith (+) n 1 m) empty ns

part1 :: String -> Int
part1 s = sum $ zipWith f as bs
  where
    [as, bs] = map sort $ parseLists s
    f a b = abs $ a - b

part2 :: String -> Int
part2 s = sum $ map f as
  where
    [as, bs] = parseLists s
    f a = a * lookup a
    lookup a = findWithDefault 0 a $ histogram bs

run :: IO ()
run = do
  text <- readFile "input/1.txt"
  print $ part1 text
  print $ part2 text
