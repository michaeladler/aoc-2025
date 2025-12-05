module Day05 where

import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, parseOnly, sepBy)
import Data.Either (fromRight)
import Data.Foldable (find)
import Data.Maybe (isJust)
import Data.Text (Text)

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
solveInternal (fresh, ingredients) =
  let part1 = length (filter id (map (\ing -> ingredientIsFresh fresh ing) ingredients))
   in (part1, 0)

ingredientIsFresh :: [Interval] -> MyInt -> Bool
ingredientIsFresh ivs ing = isJust $ find id (map (`containsIngredient` ing) ivs)

containsIngredient :: Interval -> MyInt -> Bool
containsIngredient (Interval (a, b)) x = a <= x && x <= b
 

-- Experimental Area
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
