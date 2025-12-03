module Day03 where

import Data.Attoparsec.Text
import Data.Char (digitToInt)
import Data.Text (Text)

type MyInt = Int

solve :: Text -> Either String (MyInt, MyInt)
solve content = case parseOnly parseInput content of
  Left err -> Left err
  Right result -> Right (solveInternal result)

parseInput :: Parser [[MyInt]]
parseInput = many1 (digitToInt <$> digit) `sepBy` endOfLine

solveInternal :: [[MyInt]] -> (MyInt, MyInt)
solveInternal banks =
  let part1 = foldl' (\acc bank -> acc + processBank bank) 0 banks
   in (part1, 0)

processBank :: [MyInt] -> MyInt
processBank bank = case twoLargest bank of
  Just (a, b) -> a * 10 + b
  _ -> undefined

twoLargest :: (Ord a) => [a] -> Maybe (a, a)
twoLargest (x : y : xs) = Just $ go x y xs
  where
    go max1 max2 [] = (max1, max2)
    go max1 max2 (z : zs)
      | max2 > max1 = go max2 z zs -- discard max1
      | z > max2 = go max1 z zs -- discard max2
      | otherwise = go max1 max2 zs
twoLargest _ = Nothing
