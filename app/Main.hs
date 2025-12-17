module Main where

import qualified Data.ByteString.Char8 as C
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
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
runDay 7 = solveDay Day07.solve "input/07.txt"
runDay 8 = solveDay Day08.solve "input/08.txt"
runDay 9 = solveDay Day09.solve "input/09.txt"
runDay 10 = solveDay Day10.solve "input/10.txt"
runDay 11 = solveDay Day11.solve "input/11.txt"
runDay _ = putStrLn "Day not implemented yet."

solveDay :: (Show a) => (C.ByteString -> Either String a) -> FilePath -> IO ()
solveDay solver fileName = do
  content <- C.readFile fileName
  case solver content of
    Left err -> print err
    Right ok -> print ok
