module Day05 where

import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, parseOnly, sepBy)
import Data.Text (Text)
import Data.Either (fromRight)

type MyInt = Int

newtype Interval = Interval (MyInt, MyInt)
  deriving (Eq, Show)

solve :: Text -> Either String (MyInt, MyInt)
solve content = case parseOnly parseInput content of
  Left err -> Left err
  Right result -> Right (solveInternal result)

parseInput :: Parser ([Interval], [MyInt])
parseInput = do
  fresh <- parseFresh
  _ <- endOfLine
  _ <- endOfLine
  ingredients <- parseIngredients
  pure (fresh, ingredients)

parseInterval :: Parser Interval
parseInterval = do
  a <- decimal
  _ <- char '-'
  b <- decimal
  pure $ Interval (a, b)

parseFresh :: Parser [Interval]
parseFresh = parseInterval `sepBy` endOfLine

parseIngredients :: Parser [MyInt]
parseIngredients = decimal `sepBy` endOfLine

solveInternal :: ([Interval], [MyInt]) -> (MyInt, MyInt)
solveInternal input = (0, 0)

example :: ([Interval], [MyInt])
example = fromRight ([], []) $ parseOnly parseInput exampleInput

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
