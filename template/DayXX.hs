module DayXX where

import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfLine, isSpace, many', many1, parseOnly, sepBy, sepBy1, skipSpace, takeTill)
import qualified Data.ByteString.Char8 as C
import Data.Either (fromRight)

type MyInt = Int

type Label = C.ByteString

type Targets = [C.ByteString]

type AocInput = [(Label, Targets)]

solve :: C.ByteString -> Either String (MyInt, MyInt)
solve input = solve' <$> parseOnly inputParser input

inputParser :: Parser AocInput
inputParser = lineParser `sepBy` endOfLine

lineParser :: Parser (Label, Targets)
lineParser = do
  label <- takeTill (== ':')
  _ <- char ':'
  skipSpace
  targets <- sepBy1 (takeTill isSpace) (char ' ' >> skipSpace)
  pure (label, targets)

solve' :: AocInput -> (MyInt, MyInt)
solve' input = (0, 0)

-- Experimental Area
example :: AocInput
example = fromRight [] $ parseOnly inputParser exampleInput

exampleInput :: C.ByteString
exampleInput =
  """
  aaa: you hhh
  you: bbb ccc
  bbb: ddd eee
  ccc: ddd eee fff
  ddd: ggg
  eee: out
  fff: out
  ggg: out
  hhh: ccc fff iii
  iii: out

  """
