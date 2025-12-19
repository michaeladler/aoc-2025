module DayXX where

import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfLine, isSpace, many', many1, parseOnly, sepBy, sepBy1, skipSpace, takeTill)
import qualified Data.ByteString.Char8 as C
import Protolude

type MyInt = Int

type AocInput = [[MyInt]]

solve :: C.ByteString -> Either Text (MyInt, MyInt)
solve input = solve' <$> first toS (parseOnly inputParser input)

inputParser :: Parser AocInput
inputParser = lineParser `sepBy` endOfLine

lineParser :: Parser [MyInt]
lineParser = decimal `sepBy` char ','

solve' :: AocInput -> (MyInt, MyInt)
solve' input = (0, 0)

-- Experimental Area
example :: AocInput
example = fromRight [] $ parseOnly inputParser exampleInput

exampleInput :: C.ByteString
exampleInput =
  """
  123,456
  789,111

  """
