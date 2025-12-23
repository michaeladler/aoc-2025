module Day06 (solve) where

import AocUtils
import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfLine, many', many1, sepBy, sepBy1, space)
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap as IntMap
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Protolude

type MyInt = Int64

data BinOp = Plus | Mul
  deriving (Eq, Show)

solve :: C.ByteString -> Either Text (MyInt, Maybe MyInt)
solve content =
  parseOnly parseInput content >>= \(numbers, ops) ->
    Right (solvePart1 numbers ops, solvePart2 content ops)

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
solvePart1 numbers ops = sum (zipWith opToFn ops (IntMap.elems (columns numbers)))

opToFn :: (Foldable f, Num a) => BinOp -> f a -> a
opToFn Mul = product
opToFn Plus = sum

solvePart2 :: C.ByteString -> [BinOp] -> Maybe MyInt
solvePart2 input ops = case unsnoc (C.lines input) of
  Nothing -> Nothing
  Just (ls, binops) -> sum <$> results
    where
      colPos = findColumns binops
      mx = columns (map (`splitCols` colPos) ls)
      cols = IntMap.elems mx
      results =
        let colsParsed = map parseCol cols
            zipped = zip colsParsed ops
         in Just (map (\(xs, op) -> opToFn op xs) zipped)

parseCol :: Seq C.ByteString -> [MyInt]
parseCol xs@(x :<| _) = map (extractNumber xs) [0 .. C.length x - 1]
parseCol _ = []

extractNumber :: Seq C.ByteString -> Int -> MyInt
extractNumber xs pos = let digits = map (`C.index` pos) xs in toNumber (Seq.filter (/= ' ') digits)

toNumber :: Seq Char -> MyInt
toNumber digits = fromIntegral $ foldl' (\acc x -> acc * 10 + x) 0 (map convertDigit digits)
  where
    convertDigit c = ord c - ord '0'

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
