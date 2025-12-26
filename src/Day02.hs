module Day02 where

import AocUtils
import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, sepBy)
import qualified Data.ByteString.Char8 as C
import qualified Data.IntSet as IntSet
import qualified Data.Sequence as Seq
import Protolude

solve :: C.ByteString -> Either Text (Int, Int)
solve bs = solveInternal <$> parseOnly parseRanges bs

parseRanges :: Parser [(Int, Int)]
parseRanges = parseRange `sepBy` char ','
  where
    parseRange = (,) <$> decimal <* char '-' <*> decimal

solveInternal :: [(Int, Int)] -> (Int, Int)
solveInternal xs =
  let part1 = foldl' (\acc (lower, upper) -> acc + sum (filter isInvalidID [lower .. upper])) 0 xs
      tmp = IntSet.unions (map (uncurry invalidIDsPart2) xs)
      part2 = IntSet.foldl' (+) 0 tmp
   in (part1, part2)

isInvalidID :: Int -> Bool
isInvalidID x = r == 0 && lhs == rhs
  where
    ds = digits x
    (q, r) = Seq.length ds `divMod` 2
    (lhs, rhs) = Seq.splitAt q ds

invalidIDsPart2 :: Int -> Int -> IntSet
invalidIDsPart2 lower upper =
  let buildBlock n =
        let divisors = mapMaybe (\i -> let (q, r) = n `divMod` i in if r == 0 && q >= 2 then Just (i, q) else Nothing) [1 .. n `div` 2]
         in IntSet.unions (map (helper lower upper) divisors)
      blocks = map buildBlock [numDigits lower .. numDigits upper]
   in IntSet.unions blocks

helper :: Int -> Int -> (Int, Int) -> IntSet
helper l u (n, c) = IntSet.fromDistinctAscList $ filter (\x -> l <= x && x <= u) [replicateInt a c | a <- [10 ^ (n - 1) .. 10 ^ n - 1]]
