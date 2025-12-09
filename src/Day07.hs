module Day07 (solve) where

import qualified Data.ByteString.Char8 as C
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

type MyInt = Int

type Beams = HashMap Int MyInt

solve :: C.ByteString -> Either String (MyInt, MyInt)
solve input = Right (solve' (parseInput input))

solve' :: [C.ByteString] -> (MyInt, MyInt)
solve' rows =
  let startCol = C.length (head rows) `div` 2 -- start is always at the center
      initialBeams = HashMap.singleton startCol 1 :: Beams
      (part1, beams) = foldl' processRow (0, initialBeams) rows
      part2 = sum (HashMap.elems beams)
   in (part1, part2)

processRow :: (MyInt, Beams) -> C.ByteString -> (MyInt, Beams)
processRow (count, beams) row = let result = HashMap.foldlWithKey' (f row) (count, HashMap.empty) beams in result
  where
    f :: C.ByteString -> (MyInt, Beams) -> Int -> MyInt -> (MyInt, Beams)
    f row (count, newBeams) k v =
      if C.index row k == '.'
        then (count, HashMap.insertWith (+) k v newBeams)
        else (count + 1, splitBeam k v newBeams)

splitBeam :: Int -> MyInt -> Beams -> Beams
splitBeam k v beams = HashMap.insertWith (+) (k + 1) v (HashMap.insertWith (+) (k - 1) v beams)

parseInput :: C.ByteString -> [C.ByteString]
parseInput bs = filter (not . C.null) (tail (C.split '\n' bs))
