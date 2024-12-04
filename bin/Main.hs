module Main (main) where

import Day1 (run)
import Day2 (run)
import System.Environment (getArgs)

days :: [IO ()]
days =
  [ Day1.run
  , Day2.run
  ]

runDay :: Int -> IO ()
runDay n = do
  putStrLn ("Day " ++ show n)
  days !! (n - 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [n] | day `elem` [1 .. length days] -> runDay day
      where
        day = read n
    _ -> mapM_ runDay [1 .. length days]
