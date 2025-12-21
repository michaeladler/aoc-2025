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
import qualified Day12
import Protolude

main :: IO ()
main =
  getArgs >>= \args ->
    case mapMaybe readMaybe args of
      days@(_x : _xs) -> mapM_ runDay days
      _ -> mapM_ runDay [1 .. 12]

runDay :: Int -> IO ()
runDay 1 = runSolver Day01.solve "input/01.txt"
runDay 2 = runSolver Day02.solve "input/02.txt"
runDay 3 = runSolver Day03.solve "input/03.txt"
runDay 4 = runSolver Day04.solve "input/04.txt"
runDay 5 = runSolver Day05.solve "input/05.txt"
runDay 6 = runSolver Day06.solve "input/06.txt"
runDay 7 = runSolver Day07.solve "input/07.txt"
runDay 8 = runSolver Day08.solve "input/08.txt"
runDay 9 = runSolver Day09.solve "input/09.txt"
runDay 10 = runSolver Day10.solve "input/10.txt"
runDay 11 = runSolver Day11.solve "input/11.txt"
runDay 12 = runSolver Day12.solve "input/12.txt"
runDay _ = putText "Day not implemented yet."

runSolver :: (Show a) => (C.ByteString -> Either Text a) -> FilePath -> IO ()
runSolver solver fileName = C.readFile fileName >>= \content -> either putErrText print (solver content)
