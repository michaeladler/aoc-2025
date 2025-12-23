module AocUtils
  ( choose2,
    sortDesc,
    skipUntil,
    parseOnly,
    columns,
    neighbours4,
    neighbors8,
  )
where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.IntMap as IntMap
import Data.Sequence ((><))
import qualified Data.Sequence as Seq
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

-- | Transpose a list of rows into a map of columns.
-- Each key in the resulting 'IntMap' is the column index,
-- and its value is a 'Seq' containing all elements from that column,
-- in the order they appeared in the input rows.
-- Handles ragged (non-square) input: missing values in shorter rows are ignored.
--
-- Example:
-- >>> columns [[1,2],[3,4,5]]
-- fromList [(0,fromList [1,3]), (1,fromList [2,4]), (2,fromList [5])]
columns :: [[a]] -> IntMap (Seq a)
columns xs = foldl' f mempty (map (zip [0 ..]) xs)
  where
    f = foldl' (\acc' (col, val) -> IntMap.insertWith (flip (><)) col (Seq.singleton val) acc')

-- | Returns the 4 points orthogonally adjacent to the given point.
{-# INLINE neighbours4 #-}
neighbours4 :: (Num a, Num b) => (a, b) -> [(a, b)]
neighbours4 (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

-- | Returns the 8 points orthogonally or diagonally adjacent to the given point.
{-# INLINE neighbors8 #-}
neighbors8 :: (Eq a, Eq b, Num a, Num b) => (a, b) -> [(a, b)]
neighbors8 (row, col) =
  [ (row - 1, col - 1),
    (row - 1, col),
    (row - 1, col + 1),
    (row, col - 1),
    (row, col + 1),
    (row + 1, col - 1),
    (row + 1, col),
    (row + 1, col + 1)
  ]
