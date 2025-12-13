module Day09 where

import qualified Data.ByteString.Char8 as C
import Data.Int (Int64)
import Data.Maybe (fromJust, mapMaybe)
import Debug.Trace (trace)

type MyInt = Int64

type Point = (MyInt, MyInt)

data Line
  = Horizontal (Point, MyInt)
  | Vertical (Point, MyInt)
  deriving (Eq, Show)

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
      polygonPoints = (head tiles, last tiles) : zip tiles (tail tiles)
      polygon = map convertPointsToLine polygonPoints
      -- only have to check the borders of the areas if they are inside the green/red area; use "point in polygon algorithm" from one of the previous years :)
      part1 = maximum areas
   in (part1, trace (show polygon) 0)

convertPointsToLine :: (Point, Point) -> Line
convertPointsToLine ((x, y), (x', y'))
  | y == y' = Horizontal ((min x x', y), abs (x' - x))
  | x == x' = Vertical ((x, min y y'), abs (y' - y))
  | otherwise = error "Not a line"

rectangleArea :: Point -> Point -> MyInt
rectangleArea (x, y) (x', y') =
  let dy = abs (y' - y) + 1
      dx = abs (x' - x) + 1
   in dy * dx

pointOnLine :: Point -> Line -> Bool
pointOnLine (x, y) (Horizontal ((x', y'), delta)) = y == y' && x' <= x && x <= x' + delta
pointOnLine (x, y) (Vertical ((x', y'), delta)) = x == x' && y' <= y && y <= y' + delta

-- Ray casting: To determine if a point is inside a polygon, cast a ray from the point and count edge intersections.
-- An odd count means inside; even means outside.
pointInPolygon :: Point -> Polygon -> Bool
pointInPolygon p poly = odd (length (mapMaybe (p `intersectLine`) poly))

-- Given a Point and a Line, compute the intersection point where a horizontal ray starting from the Point meets the Line.
intersectLine :: Point -> Line -> Maybe Point
intersectLine (x, y) (Horizontal ((x', y'), delta))
  | y == y' && x < x' = Just (x', y)
  | y == y' && x' <= x && x <= x' + delta = Just (x, y)
  | otherwise = Nothing
intersectLine (x, y) (Vertical ((x', y'), delta))
  | x <= x' && y >= y' && y <= y' + delta = Just (x', y)
  | otherwise = Nothing

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
