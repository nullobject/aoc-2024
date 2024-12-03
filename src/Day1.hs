module Day1 (findDigits, joinDigits, parseDigit, part1, part2, run) where

import Control.Applicative ((<|>))
import Control.Monad (msum)
import Data.List (elemIndex, find, isPrefixOf, tails)

digits :: [String]
digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

numbers :: [String]
numbers =
  [ "zero"
  , "one"
  , "two"
  , "three"
  , "four"
  , "five"
  , "six"
  , "seven"
  , "eight"
  , "nine"
  ]

parseDigit :: String -> Maybe Int
parseDigit s = elemIndex s digits <|> elemIndex s numbers

findPrefixes :: [String] -> String -> [Maybe String]
findPrefixes prefixes s = map findPrefix $ tails s
  where
    findPrefix x = find (`isPrefixOf` x) prefixes

findDigits :: [String] -> String -> Maybe (String, String)
findDigits prefixes s = do
  a <- msum $ findPrefixes prefixes s
  b <- msum $ reverse $ findPrefixes prefixes s
  return (a, b)

joinDigits :: (String, String) -> Maybe Int
joinDigits (a, b) = do
  a_ <- parseDigit a
  b_ <- parseDigit b
  return $ read (show a_ ++ show b_)

sumLines :: [String] -> [String] -> Maybe Int
sumLines prefixes text = do
  as <- mapM (findDigits prefixes) text
  bs <- mapM joinDigits as
  return $ sum bs

part1 :: String -> Maybe Int
part1 text = sumLines digits $ lines text

part2 :: String -> Maybe Int
part2 text = sumLines (digits ++ numbers) (lines text)

run :: IO ()
run = do
  text <- readFile "input/1.txt"
  print $ part1 text
  print $ part2 text
