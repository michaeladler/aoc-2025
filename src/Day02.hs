module Day02 where

import AocUtils (intToText)
import Data.Attoparsec.Text
import Data.Foldable (find)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

type MyInt = Int64

solve :: Text -> Either String (MyInt, MyInt)
solve content = case parseOnly parseRanges content of
  Left err -> Left err
  Right result -> Right (solveInternal result)

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
  let s = intToText x
      (q, r) = T.length s `divMod` 2
   in r == 0 && T.take q s == T.drop q s

invalidIDsPart2 :: (MyInt, MyInt) -> [MyInt]
invalidIDsPart2 (lower, upper) = filter isInvalidIDPart2 [lower .. upper]

isInvalidIDPart2 :: MyInt -> Bool
isInvalidIDPart2 x =
  let s = intToText x
      n = T.length s
      indices = [i | i <- [1 .. n `div` 2], n `mod` i == 0]
      candidates = map (\i -> (n `div` i, T.take i s)) indices
      candidates' = map (uncurry T.replicate) candidates
   in isJust (find (== s) candidates')
