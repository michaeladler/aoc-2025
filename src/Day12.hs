module Day12 where

import AocUtils
import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfLine, sepBy)
import Protolude

type MyInt = Int

type AocInput = [((MyInt, MyInt), [MyInt])]

solve :: ByteString -> Either Text MyInt
solve input = solve' <$> parseOnly inputParser input

inputParser :: Parser AocInput
inputParser = skipUntil lineParser <* endOfLine >>= \x -> (x :) <$> lineParser `sepBy` endOfLine
  where
    lineParser = do
      (w, h) <- (,) <$> decimal <* char 'x' <*> decimal <* char ':' <* char ' '
      shapes <- decimal `sepBy` char ' '
      pure ((w, h), shapes)

solve' :: AocInput -> MyInt
solve' input = length (filter identity (map (uncurry isRegionPossible) input))
  where
    -- assumption: each shape is at most 3x3
    isRegionPossible (w, h) shapes = 9 * sum shapes <= w * h
