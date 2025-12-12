module Day09 where

import qualified Data.ByteString.Char8 as C
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

type MyInt = Int64

type Point = (MyInt, MyInt)

type Line = (Point, Point)

type Polygon = [Line]

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
      polygon = (head tiles, last tiles) : zip tiles (tail tiles)
      -- only have to check the borders of the areas if they are inside the green/red area; use "point in polygon algorithm" from one of the previous years :)
      part1 = maximum areas
   in (part1, trace (show polygon) 0)

rectangleArea :: Point -> Point -> MyInt
rectangleArea (x, y) (x', y') =
  let dy = abs (y' - y) + 1
      dx = abs (x' - x) + 1
   in dy * dx

pointOnLine :: Point -> Line -> Bool
pointOnLine (x, y) ((px, py), (qx, qy))
  | px == qx =
      let miny = min py qy
          maxy = max py qy
       in miny <= y && y <= maxy -- vertical line
  | py == qy =
      let minx = min px qx
          maxx = max px qx
       in minx <= x && x <= maxx -- horizontal line
  | otherwise = error "Not a vertical or horizontal line"

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
