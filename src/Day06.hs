module Day06 (solve) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfLine, many', many1, parseOnly, sepBy, sepBy1, space)
import qualified Data.ByteString.Char8 as C
import Data.Int (Int64)
import qualified Data.Matrix as Matrix
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude hiding (lines)

type MyInt = Int64

data BinOp = Plus | Mul
  deriving (Eq, Show)

solve :: C.ByteString -> Either String (MyInt, MyInt)
solve content = case parseOnly parseInput content of
  Left err -> Left err
  Right (numbers, ops) -> Right (solvePart1 numbers ops, solvePart2 content ops)

parseInput :: Parser ([[MyInt]], [BinOp])
parseInput = do
  numbers <- parseNumbers `sepBy` (many' (char ' ') >> endOfLine)
  _ <- many' (char ' ') >> endOfLine >> many' (char ' ')
  ops <- parseBinOp `sepBy` many1 space
  return (numbers, ops)

parseNumbers :: Parser [MyInt]
parseNumbers = many' (char ' ') >> decimal `sepBy1` many1 (char ' ')

parseBinOp :: Parser BinOp
parseBinOp = (char '+' >> pure Plus) <|> (char '*' >> pure Mul)

solvePart1 :: [[MyInt]] -> [BinOp] -> MyInt
solvePart1 numbers ops =
  let mx = Matrix.fromLists numbers
      computeCol op j = case op of
        Mul -> foldl' (*) 1 (Matrix.getCol j mx)
        Plus -> foldl' (+) 0 (Matrix.getCol j mx)
      ops' = zip ops [1 ..]
      results = map (uncurry computeCol) ops'
   in sum results

solvePart2 :: C.ByteString -> [BinOp] -> MyInt
solvePart2 input ops =
  let tmp = C.lines input
      lines = init tmp
      binops = last tmp
      colPos = findColumns binops
      mx = Matrix.fromLists $ map (`splitCols` colPos) lines
      cols = map (`Matrix.getCol` mx) [1 .. Matrix.ncols mx]
      colsParsed = map parseCol cols
      colsParsed' = zip colsParsed ops
      calc (xs, op) = case op of
        Mul -> foldl' (*) 1 xs
        Plus -> foldl' (+) 0 xs
      results = map calc colsParsed'
   in sum results

parseCol :: Vector C.ByteString -> [MyInt]
parseCol xs = if not (V.null xs) then map (extractNumber (V.toList xs)) [0 .. C.length (V.head xs) - 1] else []

extractNumber :: [C.ByteString] -> Int -> MyInt
extractNumber xs pos = read $ map (`C.index` pos) xs

-- splitCols "123 328  51 64" [0,4,8,12] == ["123","328"," 51","64"]
splitCols :: C.ByteString -> [Int] -> [C.ByteString]
splitCols bs [] = [bs]
splitCols bs [i] = [C.drop i bs]
splitCols bs (i : j : xs) = slice i (max 0 (j - 1)) bs : splitCols bs (j : xs)

-- could also use position of binops in last column to get start of a column
findColumns :: C.ByteString -> [Int]
findColumns bs =
  let zipped = zip (C.unpack bs) [0 .. C.length bs]
      filtered = filter (\(c, _) -> c == '*' || c == '+') zipped
   in map snd filtered

slice :: Int -> Int -> C.ByteString -> C.ByteString
slice i j b = C.drop i (C.take j b)
