{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Day2 (checkReport, pairs, parseReports, part1, run) where

pairs :: [Int] -> [(Int, Int)]
pairs ns = zipWith (,) ns (tail ns)

parseReports :: String -> [[Int]]
parseReports = map f . lines
  where
    f = map read . words

checkReport :: [Int] -> Bool
checkReport ns = allInc || allDec
  where
    allInc = all (\n -> n > 0 && n <= 3) deltas
    allDec = all (\n -> n < 0 && n >= -3) deltas
    deltas = map (uncurry (-)) $ pairs ns

part1 :: String -> Int
part1 s = length $ filter checkReport $ parseReports s

run :: IO ()
run = do
  text <- readFile "input/2.txt"
  print $ part1 text
