module Day02 (solve, invalidIDs, isInvalidID) where

import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, parseOnly, sepBy)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import Protolude

type MyInt = Int

solve :: C.ByteString -> Either Text (MyInt, MyInt)
solve = fmap solveInternal . first toS . parseOnly parseRanges

-- Parse a single range like "11-22"
parseRange :: Parser (MyInt, MyInt)
parseRange = do
  start <- decimal
  _ <- char '-'
  end <- decimal
  return (start, end)

-- Parse the full line of comma-separated ranges
parseRanges :: Parser [(MyInt, MyInt)]
parseRanges = parseRange `sepBy` char ','

solveInternal :: [(MyInt, MyInt)] -> (MyInt, MyInt)
solveInternal xs =
  let part1 = foldl' (\acc range -> acc + sum (invalidIDs range)) 0 xs
      part2 = foldl' (\acc range -> acc + sum (invalidIDsPart2 range)) 0 xs
   in (part1, part2)

invalidIDs :: (MyInt, MyInt) -> [MyInt]
invalidIDs (lower, upper) = filter isInvalidID [lower .. upper]

isInvalidID :: MyInt -> Bool
isInvalidID x =
  let s = intToLazyBS x
      (q, r) = BL.length s `divMod` 2
   in r == 0 && BL.take q s == BL.drop q s

invalidIDsPart2 :: (MyInt, MyInt) -> [MyInt]
invalidIDsPart2 (lower, upper) = filter isInvalidIDPart2 [lower .. upper]

isInvalidIDPart2 :: MyInt -> Bool
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
