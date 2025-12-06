module Main where

import qualified Data.ByteString.Char8 as C
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
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
runDay 1 = solveDay Day01.solve "input/01.txt"
runDay 2 = solveDay Day02.solve "input/02.txt"
runDay 3 = solveDay Day03.solve "input/03.txt"
runDay 4 = solveDay Day04.solve "input/04.txt"
runDay 5 = solveDay Day05.solve "input/05.txt"
runDay 6 = solveDay Day06.solve "input/06.txt"
runDay _ = putStrLn "Day not implemented yet."

solveDay :: (Show a) => (C.ByteString -> Either String a) -> FilePath -> IO ()
solveDay solver fileName = do
  content <- C.readFile fileName
  case solver content of
    Left err -> print err
    Right ok -> print ok
