module Day03 (solve) where

import Data.Attoparsec.ByteString.Char8 (Parser, digit, endOfLine, many1, parseOnly, sepBy)
import qualified Data.ByteString.Char8 as C
import qualified Data.List as List
import Protolude

type MyInt = Int

solve :: C.ByteString -> Either Text (MyInt, MyInt)
solve content = solveInternal <$> first toS (parseOnly parseInput content)

parseInput :: Parser [[MyInt]]
parseInput = many1 (digitToInt <$> digit) `sepBy` endOfLine

solveInternal :: [[MyInt]] -> (MyInt, MyInt)
solveInternal banks = (helper 2, helper 12)
  where
    helper k = foldl' (\acc bank -> acc + listToNumber (kLargest k bank)) 0 banks

listToNumber :: [MyInt] -> MyInt
listToNumber = foldl' (\acc d -> acc * 10 + d) 0

kLargest :: Int -> [MyInt] -> [MyInt]
kLargest 0 _ = []
kLargest k bank =
  let takeUntil = length bank - k + 1
      (maxVal, pos) = maxWithPos (List.take takeUntil bank)
      restBank = drop (pos + 1) bank
   in maxVal : kLargest (k - 1) restBank

maxWithPos :: [MyInt] -> (Int, Int)
maxWithPos bank = List.minimumBy (\lhs rhs -> fst rhs `compare` fst lhs) (zip bank [0 ..])
