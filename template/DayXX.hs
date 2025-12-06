module DayXX where

import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, parseOnly, sepBy)
import Data.Text (Text)
import Data.Either (fromRight)

type MyInt = Int

solve :: Text -> Either String (MyInt, MyInt)
solve content = case AP.parseOnly parseInput content of
  Left err -> Left err
  Right result -> Right (solveInternal result)

parseInput :: Parser [[MyInt]]
parseInput = many1 AP.decimal `sepBy` endOfLine

solveInternal :: [[MyInt]] -> (MyInt, MyInt)
solveInternal input = (0, 0)

-- Experimental Area
example :: [[MyInt]]
example = fromRight [] $ parseOnly parseInput exampleInput

exampleInput :: Text
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
