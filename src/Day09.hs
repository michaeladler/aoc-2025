module Day09 where

import qualified Data.ByteString.Char8 as C
import Data.Int (Int64)
import Data.Maybe (fromJust)

type MyInt = Int64

type Point = (MyInt, MyInt)

solve :: C.ByteString -> Either String (MyInt, MyInt)
solve content = Right (solveInternal (inputParser content))

inputParser :: C.ByteString -> [Point]
inputParser bs = map parsePoint (filter (not . C.null) (C.split '\n' bs))
  where
    parsePoint bs' =
      case C.split ',' bs' of
        x : y : _ -> (fst (fromJust (C.readInt64 x)), fst (fromJust (C.readInt64 y)))
        _ -> error $ "Invalid input: " ++ show bs'

solveInternal :: [Point] -> (MyInt, MyInt)
solveInternal tiles =
  let areas = [rectangleArea p q | p <- tiles, q <- tiles, p /= q]
      part1 = maximum areas
   in (part1, 0)

rectangleArea :: Point -> Point -> MyInt
rectangleArea (x, y) (x', y') =
  let dy = abs (y' - y) + 1
      dx = abs (x' - x) + 1
   in dy * dx

-- Experimental Area
example :: [Point]
example = inputParser exampleInput

exampleInput :: C.ByteString
exampleInput =
  """
  7,1
  11,1
  11,7
  9,7
  9,5
  2,5
  2,3
  7,3

  """
