module Day09 where

import qualified Data.ByteString.Char8 as C
import qualified Data.HashSet as HS
import Data.Int (Int64)
import Data.Maybe (fromJust, mapMaybe)

type MyInt = Int64

type Point = (MyInt, MyInt)

data Line
  = Horizontal (Point, MyInt)
  | Vertical (Point, MyInt)
  deriving (Show)

newtype Rectangle = Rectangle (Point, Point)
  deriving (Show)

type Polygon = [Line]

-- part2: 30872120 too low :(
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
  let rectangles = [Rectangle (p, q) | p <- tiles, q <- tiles, p /= q]
      part1 = maximum [rectangleArea r | r <- rectangles]

      polygon = buildPolygon tiles
      filteredRectangles = filter (`isRectangleInsidePolygon` polygon) rectangles
      part2 = maximum [rectangleArea r | r <- filteredRectangles]
   in (part1, part2)

rectangleArea :: Rectangle -> MyInt
rectangleArea (Rectangle ((x, y), (x', y'))) =
  let dy = abs (y' - y) + 1
      dx = abs (x' - x) + 1
   in dy * dx

buildPolygon :: [Point] -> [Line]
buildPolygon tiles = map toLine ((head tiles, last tiles) : zip tiles (tail tiles))
  where
    toLine ((x, y), (x', y'))
      | y == y' = Horizontal ((min x x', y), abs (x' - x))
      | x == x' = Vertical ((x, min y y'), abs (y' - y))
      | otherwise = error "Not a line"

isRectangleInsidePolygon :: Rectangle -> Polygon -> Bool
isRectangleInsidePolygon r poly = all (`pointInPolygon` poly) (rectanglePoints r)

rectanglePoints :: Rectangle -> [Point]
rectanglePoints (Rectangle ((x, y), (x', y'))) =
  let xmin = min x x'
      xmax = max x x'
      ymin = min y y'
      ymax = max y y'
      left = [(xv, ymin) | xv <- [xmin .. xmax]]
      right = [(xv, ymax) | xv <- [xmin .. xmax]]
      top = [(xmin, yv) | yv <- [ymin + 1 .. ymax - 1]]
      bottom = [(xmax, yv) | yv <- [ymin + 1 .. ymax - 1]]
   in top ++ bottom ++ left ++ right

-- Ray casting: To determine if a point is inside a polygon, cast a ray from the point and count edge intersections.
-- An odd count means inside; even means outside.
pointInPolygon :: Point -> Polygon -> Bool
pointInPolygon p poly =
  let pointIsBorder = any (pointOnLine p) poly
   in (pointIsBorder || odd (HS.size (HS.fromList (mapMaybe (p `intersectLine`) poly))))

-- Given a Point and a Line, compute the intersection point where a horizontal ray starting from the Point meets the Line.
intersectLine :: Point -> Line -> Maybe Point
intersectLine p@(x, y) (Horizontal ((x', y'), delta))
  | y == y' && x < x' = Just (x', y') -- point is left of horizontal line
  | y == y' && x' <= x && x <= x' + delta = Just p -- point is on horizontal line
  | otherwise = Nothing -- point is right of horizontal line
intersectLine (x, y) (Vertical ((x', y'), delta))
  | x <= x' && y >= y' && y <= y' + delta = Just (x', y)
  | otherwise = Nothing

pointOnLine :: Point -> Line -> Bool
pointOnLine (x, y) (Horizontal ((x', y'), delta)) = y == y' && x' <= x && x <= x' + delta
pointOnLine (x, y) (Vertical ((x', y'), delta)) = x == x' && y' <= y && y <= y' + delta

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

-- r = Rectangle ( (2, 5), (7 ,3)  )
-- polygon = buildPolygon example
-- points = sort $ rectanglePoints r
-- map (`pointInPolygon` polygon) points
