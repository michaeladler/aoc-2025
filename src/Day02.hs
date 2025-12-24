module Day02 where

import AocUtils
import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, sepBy)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
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
  let part1 = foldl' (\acc range -> acc + sum (invalidIDs range)) 0 xs
      part2 = foldl' (\acc range -> acc + sum (invalidIDsPart2 range)) 0 xs
   in (part1, part2)

invalidIDs :: (Int, Int) -> [Int]
invalidIDs (lower, upper) = filter isInvalidID [lower .. upper]

isInvalidID :: Int -> Bool
isInvalidID x =
  let ds = digits x
      (q, r) = Seq.length ds `divMod` 2
      (lhs, rhs) = Seq.splitAt q ds
   in r == 0 && lhs == rhs

invalidIDsPart2 :: (Int, Int) -> [Int]
invalidIDsPart2 (lower, upper) = filter isInvalidIDPart2 [lower .. upper]

isInvalidIDPart2 :: Int -> Bool
isInvalidIDPart2 x =
  let s = intToLazyBS x
      n = fromIntegral (BL.length s)
      indices = [i | i <- [1 .. n `div` 2], n `mod` i == 0]
      candidates = map (\i -> (n `div` i, BL.take i s)) indices
      candidates' = map (\(i, bs) -> replicateBS (fromIntegral i) bs) candidates
   in isJust (find (== s) candidates')

intToLazyBS :: Int -> BL.ByteString
intToLazyBS = BB.toLazyByteString . BB.intDec

replicateBS :: Int -> BL.ByteString -> BL.ByteString
replicateBS n bs = case n of
  0 -> mempty
  1 -> bs
  _ -> bs <> replicateBS (n - 1) bs
