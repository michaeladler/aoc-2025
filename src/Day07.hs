module Day07 (solve) where

import qualified Data.ByteString.Char8 as C
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

type MyInt = Int

type Beams = IntMap MyInt

solve :: C.ByteString -> Either String (MyInt, MyInt)
solve input = Right (solve' (parseInput input))

solve' :: [C.ByteString] -> (MyInt, MyInt)
solve' rows =
  let startCol = C.length (head rows) `div` 2 -- start is always at the center
      initialBeams = IntMap.singleton startCol 1 :: Beams
      (part1, beams) = foldl' processRow (0, initialBeams) rows
      part2 = sum (IntMap.elems beams)
   in (part1, part2)

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
parseInput bs = filter (not . C.null) (tail (C.split '\n' bs))
