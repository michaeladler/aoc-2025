module Day07 (solve) where

import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IntMap
import Protolude

type MyInt = Int

type Beams = IntMap MyInt

solve :: C.ByteString -> Either Text (MyInt, MyInt)
solve input = maybe (Left "No solution found") Right (solve' (parseInput input))

solve' :: [C.ByteString] -> Maybe (MyInt, MyInt)
solve' rows@(x : _) =
  let startCol = C.length x `div` 2 -- start is always at the center
      initialBeams = IntMap.singleton startCol 1 :: Beams
      (part1, beams) = foldl' processRow (0, initialBeams) rows
      part2 = sum (IntMap.elems beams)
   in Just (part1, part2)
solve' _ = Nothing

processRow :: (MyInt, Beams) -> C.ByteString -> (MyInt, Beams)
processRow (count, beams) row = let result = IntMap.foldlWithKey' (f row) (count, IntMap.empty) beams in result
  where
    f :: C.ByteString -> (MyInt, Beams) -> Int -> MyInt -> (MyInt, Beams)
    f row (count, newBeams) k v =
      if C.index row k == '.'
        then (count, IntMap.insertWith (+) k v newBeams)
        else (count + 1, splitBeam k v newBeams)

splitBeam :: Int -> MyInt -> Beams -> Beams
splitBeam k v beams = IntMap.insertWith (+) (k + 1) v (IntMap.insertWith (+) (k - 1) v beams)

parseInput :: C.ByteString -> [C.ByteString]
parseInput bs = case C.split '\n' bs of
  (_ : xs) -> filter (not . C.null) xs
  _ -> []
