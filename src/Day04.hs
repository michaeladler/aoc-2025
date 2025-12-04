module Day04 where

import Data.Attoparsec.Text (Parser, endOfLine, many1, sepBy)
import qualified Data.Attoparsec.Text as AP
import Data.Text (Text)

type MyInt = Int

solve :: Text -> Either String (MyInt, MyInt)
solve content = case AP.parseOnly parseInput content of
  Left err -> Left err
  Right result -> Right (solveInternal result)

parseInput :: Parser [[MyInt]]
parseInput = many1 AP.decimal `sepBy` endOfLine

solveInternal :: [[MyInt]] -> (MyInt, MyInt)
solveInternal input = (0, 0)
