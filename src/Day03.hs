module Day03 where

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

-- Idea: Top-down approach.
-- Take each number in the list as a candidate for the leftmost digit.
-- Let's say the candidate is at position i.
-- Then consider the sublist starting at position i+1 and find the (k-1)th largest number.
-- For each constructed number (candidate + (k-1)th largest), use the maximum value.
kLargest :: Int -> [MyInt] -> [MyInt]
kLargest k xs
  | k == 1 && not (null xs) = [maximum xs]
  | k > 1 && length xs >= k =
      let toCheck = getPairs xs
          candidates = filter (\ys -> length ys == k) $ map (\(y, ys) -> y : kLargest (k - 1) ys) toCheck
          candidates' = List.sortBy (\lhs rhs -> listToNumber rhs `compare` listToNumber lhs) candidates
       in head candidates'
  | otherwise = []

-- | Given a list of integers, create a list of pairs.
-- For each element in the input list, the pair consists of the element itself
-- and a list containing all subsequent elements (the suffix of the original
-- list starting from the next position).
--
-- For example:
-- >>> getPairs [1,2,3,4]
-- [(1,[2,3,4]),(2,[3,4]),(3,[4]),(4,[])]
--
-- @param xs The input list of integers.
-- @return A list of tuples, where each tuple is (element, suffix_list).
getPairs :: [a] -> [(a, [a])]
getPairs [] = []
getPairs (x : xs) = (x, xs) : getPairs xs
