module AocUtils
  ( choose2,
    sortDesc,
    skipUntil,
    parseOnly,
  )
where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Protolude

-- | Generate all unique unordered pairs from a list.
-- Each pair (x, y) consists of two distinct elements from the input list,
-- with each combination appearing only once (order does not matter).
choose2 :: [a] -> [(a, a)]
choose2 xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

sortDesc :: (Ord a) => [a] -> [a]
sortDesc = sortBy (comparing Down)

-- | Skip input until 'p' succeeds, then return its result.
skipUntil :: Parser a -> Parser a
skipUntil p = p <|> (AP.anyChar *> skipUntil p)

parseOnly :: Parser a -> ByteString -> Either Text a
parseOnly p = first toS . AP.parseOnly p
