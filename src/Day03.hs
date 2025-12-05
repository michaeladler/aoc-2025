module Day03 (solve) where

import Data.Attoparsec.Text
import Data.Char (digitToInt)
import qualified Data.List as List
import Data.Text (Text)

type MyInt = Int

solve :: Text -> Either String (MyInt, MyInt)
solve content = case parseOnly parseInput content of
  Left err -> Left err
  Right result -> Right (solveInternal result)

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
