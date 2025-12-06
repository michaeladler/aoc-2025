module Day05 (solve) where

import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfLine, parseOnly, sepBy)
import qualified Data.ByteString.Char8 as C
import Data.Foldable (find)
import Data.Maybe (isJust)

type MyInt = Int

newtype Interval = Interval (MyInt, MyInt)
  deriving (Eq, Show)

solve :: C.ByteString -> Either String (MyInt, MyInt)
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
  let part1 = length (filter id (map (ingredientIsFresh fresh) ingredients))
   in -- Strategy part2: Apply reduceIntervals until there is no change, then use countCoverage:
      (part1, countCoverage (reduceIntervalsRec fresh))

ingredientIsFresh :: [Interval] -> MyInt -> Bool
ingredientIsFresh ivs ing = isJust $ find id (map (`containsIngredient` ing) ivs)

containsIngredient :: Interval -> MyInt -> Bool
containsIngredient (Interval (a, b)) x = a <= x && x <= b

isOverlap :: Interval -> Interval -> Bool
isOverlap (Interval (start1, end1)) (Interval (start2, end2)) = max start1 start2 <= min end1 end2

reduceIntervalsRec :: [Interval] -> [Interval]
reduceIntervalsRec xs =
  let n = length xs
      reduced = reduceIntervals xs
      n' = length reduced
   in if n == n' then xs else reduceIntervalsRec reduced

reduceIntervals :: [Interval] -> [Interval]
reduceIntervals = foldl' insert []
  where
    insert :: [Interval] -> Interval -> [Interval]
    insert [] iv = [iv]
    insert (x : xs) iv = if isOverlap x iv then mergeIntervals x iv : xs else x : insert xs iv

mergeIntervals :: Interval -> Interval -> Interval
mergeIntervals (Interval (start1, end1)) (Interval (start2, end2)) = Interval (min start1 start2, max end1 end2)

countCoverage :: [Interval] -> MyInt
countCoverage = foldl' (\acc (Interval (a, b)) -> acc + (b - a + 1)) 0
