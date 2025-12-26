module Day02 where

import AocUtils
import Data.Attoparsec.ByteString.Char8 (char, decimal, sepBy)
import qualified Data.ByteString.Char8 as C
import qualified Data.IntSet as IntSet
import Protolude

solve :: C.ByteString -> Either Text (Int, Int)
solve bs = solveInternal <$> parseOnly parseRanges bs
  where
    parseRanges = ((,) <$> decimal <* char '-' <*> decimal) `sepBy` char ','

solveInternal :: [(Int, Int)] -> (Int, Int)
solveInternal xs =
  let part1 = IntSet.foldl' (+) 0 (IntSet.unions (map (uncurry invalidIDsPart1) xs))
      part2 = IntSet.foldl' (+) 0 (IntSet.unions (map (uncurry invalidIDsPart2) xs))
   in (part1, part2)

invalidIDsPart1 :: Int -> Int -> IntSet
invalidIDsPart1 lower upper =
  let buildBlock n =
        let (q, r) = n `divMod` 2
         in if r == 0 then helper lower upper (q, 2) else mempty
      blocks = map buildBlock [numDigits lower .. numDigits upper]
   in IntSet.unions blocks

invalidIDsPart2 :: Int -> Int -> IntSet
invalidIDsPart2 lower upper =
  let buildBlock n =
        let divisors = mapMaybe (\i -> let (q, r) = n `divMod` i in if r == 0 && q >= 2 then Just (i, q) else Nothing) [1 .. n `div` 2]
         in IntSet.unions (map (helper lower upper) divisors)
      blocks = map buildBlock [numDigits lower .. numDigits upper]
   in IntSet.unions blocks

helper :: Int -> Int -> (Int, Int) -> IntSet
helper l u (n, c) = IntSet.fromDistinctAscList $ filter (\x -> l <= x && x <= u) [replicateInt a c | a <- [10 ^ (n - 1) .. 10 ^ n - 1]]
