module Day09 (solve) where

import AocUtils (choose2)
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromJust)
import Protolude

type Point = (Int, Int)

newtype Rectangle = Rectangle (Point, Point)
  deriving (Show)

type Polygon = (IntMap [Point], IntMap [Point]) -- (vertical, horizontal) lines; each point represents start and endpoint (inclusive)

type RasterizedPolygon = (IntMap IntSet, IntMap IntSet) -- polygon with lines replaced by individual points

solve :: C.ByteString -> Either Text (Int64, Int64)
solve content = do
  input <- inputParser content
  case solveInternal input of
    (Just a, Just b) -> Right (a, b)
    _ -> Left "No solution found"

inputParser :: C.ByteString -> Either Text [Point]
inputParser bs = mapM parsePoint (filter (not . C.null) (C.split '\n' bs))
  where
    parsePoint bs' =
      case C.split ',' bs' of
        x : y : _ -> Right (fst (fromJust (C.readInt x)), fst (fromJust (C.readInt y)))
        _ -> Left "Invalid input"

solveInternal :: [Point] -> (Maybe Int64, Maybe Int64)
solveInternal tiles =
  let rectanglesWithArea = sortBy (\x x' -> comparing Down (fst x) (fst x')) [(rectangleArea r, r) | r <- map Rectangle (choose2 tiles)]
      part1 = fst <$> head rectanglesWithArea

      poly = buildPolygon tiles
      part2 = case poly of
        Just poly' -> Just $ fst <$> head (filter (\(_, r@(Rectangle ((x, y), (x', y')))) -> (isPointOnBoundary rasterized (x, y') || isPointOnBoundary rasterized (x', y)) && hasIntersection poly' r) rectanglesWithArea)
          where
            rasterized = rasterize poly'
        Nothing -> Nothing
   in (part1, join part2)

rectangleArea :: Rectangle -> Int64
rectangleArea (Rectangle ((x, y), (x', y'))) =
  let dy = fromIntegral $ abs (y' - y) + 1
      dx = fromIntegral $ abs (x' - x) + 1
   in dy * dx

buildPolygon :: [Point] -> Maybe Polygon
buildPolygon tiles@(x : xs) = case lastMay tiles of
  Just lastTile -> Just $ foldl' f (IntMap.empty, IntMap.empty) ((x, lastTile) : zip tiles xs)
  _ -> Nothing
  where
    f (vert, horiz) ((x, y), (x', y'))
      | x == x' = (IntMap.alter (upsert (min y y', max y y')) x vert, horiz) -- vertical line
      | otherwise = (vert, IntMap.alter (upsert (min x x', max x x')) y horiz) -- horizontal line
    upsert p old = case old of
      Nothing -> Just [p]
      Just xs -> Just (p : xs)
buildPolygon _ = Nothing

rasterize :: Polygon -> RasterizedPolygon
rasterize (vert, horiz) = (IntMap.foldlWithKey' f IntMap.empty vert, IntMap.foldlWithKey' f IntMap.empty horiz)
  where
    f :: IntMap IntSet -> Int -> [Point] -> IntMap IntSet
    f acc k points = IntMap.alter (f' points) k acc

    f' :: [Point] -> Maybe IntSet -> Maybe IntSet
    f' points old =
      let pointsExpanded = concatMap expandPoint points :: [Int]
       in case old of
            Nothing -> Just (foldl' (flip IntSet.insert) IntSet.empty pointsExpanded)
            Just set -> Just (foldl' (flip IntSet.insert) set pointsExpanded)

isPointOnBoundary :: RasterizedPolygon -> Point -> Bool
isPointOnBoundary (vy, hx) (x, y) =
  let b1 = case IntMap.lookup y hx of
        Just points -> IntSet.member x points
        Nothing -> False
      b2 = case IntMap.lookup x vy of
        Just points -> IntSet.member y points
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
          (IntMap.toList horizLines)
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
          (IntMap.toList vertLines)
   in not (horizontalBlock || verticalBlock)

expandPoint :: Point -> [Int]
expandPoint (a, b) = [a .. b]
