module Main where

import qualified Data.Text.IO as TIO
import Day01
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (dayStr : _) ->
      case reads dayStr of
        [(day, _)] -> runDay day
        _ -> putStrLn "Invalid day argument. Please provide a valid integer."
    _ -> putStrLn "Please provide a day number as the first argument."

runDay :: Int -> IO ()
runDay 1 = day01
runDay _ = putStrLn "Day not implemented yet."

day01 :: IO ()
day01 = do
  content <- TIO.readFile "input/01.txt"
  case Day01.solve content of
    Left err -> print err
    Right ok -> print ok
