module Day09 (solve) where

import AocUtils (choose2)
import qualified Data.ByteString.Char8 as C
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Int (Int64)
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Data.Ord (Down (..), comparing)

type MyInt = Int64

type Point = (MyInt, MyInt)

newtype Rectangle = Rectangle (Point, Point)
  deriving (Show)

type Polygon = (HashMap MyInt [Point], HashMap MyInt [Point]) -- (vertical, horizontal) lines; each point represents start and endpoint (inclusive)

type RasterizedPolygon = (HashMap MyInt (HashSet MyInt), HashMap MyInt (HashSet MyInt)) -- polygon with lines replaced by individual points

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
  let rectanglesWithArea = sortBy (\x x' -> comparing Down (fst x) (fst x')) [(rectangleArea r, r) | r <- map Rectangle (choose2 tiles)]
      part1 = fst (head rectanglesWithArea)

      poly = buildPolygon tiles
      rasterized = rasterize poly
      part2 = fst (head (filter (\(_, r@(Rectangle ((x, y), (x', y')))) -> (isPointOnBoundary rasterized (x, y') || isPointOnBoundary rasterized (x', y)) && hasIntersection poly r) rectanglesWithArea))
   in (part1, part2)

rectangleArea :: Rectangle -> MyInt
rectangleArea (Rectangle ((x, y), (x', y'))) =
  let dy = abs (y' - y) + 1
      dx = abs (x' - x) + 1
   in dy * dx

buildPolygon :: [Point] -> Polygon
buildPolygon tiles = foldl' f (HashMap.empty, HashMap.empty) pairs
  where
    pairs = (head tiles, last tiles) : zip tiles (tail tiles)
    f (vert, horiz) ((x, y), (x', y'))
      | x == x' = (HashMap.alter (upsert (min y y', max y y')) x vert, horiz) -- vertical line
      | otherwise = (vert, HashMap.alter (upsert (min x x', max x x')) y horiz) -- horizontal line
    upsert p old = case old of
      Nothing -> Just [p]
      Just xs -> Just (p : xs)

rasterize :: Polygon -> RasterizedPolygon
rasterize (vert, horiz) = (HashMap.foldlWithKey' f HashMap.empty vert, HashMap.foldlWithKey' f HashMap.empty horiz)
  where
    f :: HashMap MyInt (HashSet MyInt) -> MyInt -> [Point] -> HashMap MyInt (HashSet MyInt)
    f acc k points = HashMap.alter (f' points) k acc

    f' :: [Point] -> Maybe (HashSet MyInt) -> Maybe (HashSet MyInt)
    f' points old =
      let pointsExpanded = concatMap expandPoint points :: [MyInt]
       in case old of
            Nothing -> Just (foldl' (flip HashSet.insert) HashSet.empty pointsExpanded)
            Just set -> Just (foldl' (flip HashSet.insert) set pointsExpanded)

isPointOnBoundary :: RasterizedPolygon -> Point -> Bool
isPointOnBoundary (vy, hx) (x, y) =
  let b1 = case HashMap.lookup y hx of
        Just points -> HashSet.member x points
        Nothing -> False
      b2 = case HashMap.lookup x vy of
        Just points -> HashSet.member y points
        Nothing -> False
   in b1 || b2

hasIntersection :: Polygon -> Rectangle -> Bool
hasIntersection (vertLines, horizLines) (Rectangle ((x, y), (u, v))) =
  let xrMin = min x u + 1
      xrMax = max x u - 1
      yrMin = min y v + 1
      yrMax = max y v - 1
      -- Check horizontal lines
      horizontalBlock =
        any
          ( \(iy, segs) ->
              iy >= yrMin
                && iy <= yrMax
                && any
                  ( \(ix1, ix2) ->
                      ix2 > xrMin && ix1 < xrMax
                  )
                  segs
          )
          (HashMap.toList horizLines)
      -- Check vertical lines
      verticalBlock =
        any
          ( \(ix, segs) ->
              ix >= xrMin
                && ix <= xrMax
                && any
                  ( \(iy1, iy2) ->
                      iy2 > yrMin && iy1 < yrMax
                  )
                  segs
          )
          (HashMap.toList vertLines)
   in not (horizontalBlock || verticalBlock)

expandPoint :: Point -> [MyInt]
expandPoint (a, b) = [a .. b]
