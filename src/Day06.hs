module Day06 where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, many', many1, parseOnly, sepBy, sepBy1, space)
import Data.Either (fromRight)
import Data.Int (Int64)
import qualified Data.Matrix as Matrix
import Data.Text (Text)

type MyInt = Int64

data BinOp = Plus | Mul
  deriving (Eq, Show)

solve :: Text -> Either String (MyInt, MyInt)
solve content = case parseOnly parseInput content of
  Left err -> Left err
  Right result -> Right (solveInternal result)

parseInput :: Parser ([[MyInt]], [BinOp])
parseInput = do
  numbers <- parseNumbers `sepBy` endOfLine
  _ <- endOfLine >> many' (char ' ')
  ops <- parseBinOp `sepBy` many1 space
  return (numbers, ops)

parseNumbers :: Parser [MyInt]
parseNumbers = many' (char ' ') >> decimal `sepBy1` many1 (char ' ')

parseBinOp :: Parser BinOp
parseBinOp = (char '+' >> pure Plus) <|> (char '*' >> pure Mul)

solveInternal :: ([[MyInt]], [BinOp]) -> (MyInt, MyInt)
solveInternal (numbers, ops) = (solvePart1 numbers ops, 0)

solvePart1 :: [[MyInt]] -> [BinOp] -> MyInt
solvePart1 numbers ops =
  let mx = Matrix.fromLists numbers
      computeCol op j = case op of
        Mul -> foldl' (*) 1 (Matrix.getCol j mx)
        Plus -> foldl' (+) 0 (Matrix.getCol j mx)
      ops' = zip ops [1 ..]
      results = map (uncurry computeCol) ops'
   in sum results

-- Experimental Area
example :: ([[MyInt]], [BinOp])
example = fromRight ([], []) $ parseOnly parseInput exampleInput

exampleInput :: Text
exampleInput =
  """
  123 328  51 64
   45 64  387 23
    6 98  215 314
  *   +   *   +

  """
