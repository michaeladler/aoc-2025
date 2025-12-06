module DayXX (solve) where

import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfLine, many', parseOnly, sepBy, many1)
import Data.Either (fromRight)
import qualified Data.ByteString.Char8 as C

type MyInt = Int

solve :: C.ByteString -> Either String (MyInt, MyInt)
solve content = case parseOnly inputParser content of
  Left err -> Left err
  Right result -> Right (solveInternal result)

inputParser :: Parser [[MyInt]]
inputParser = many1 decimal `sepBy` endOfLine

solveInternal :: [[MyInt]] -> (MyInt, MyInt)
solveInternal input = (0, 0)

-- Experimental Area
example :: [[MyInt]]
example = fromRight [] $ parseOnly inputParser exampleInput

exampleInput :: C.ByteString
exampleInput =
  """
  3-5
  10-14
  16-20
  12-18

  1
  5
  8
  11
  17
  32

  """
