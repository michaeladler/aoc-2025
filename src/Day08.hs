module Day08 (solve, solve', inputParser) where

import AocUtils (choose2, sortDesc)
import qualified Data.ByteString.Char8 as C
import qualified Data.DisjointSet as DS
import Data.Int (Int64)
import Data.List (sort)
import Data.Maybe (mapMaybe)

newtype Point = Point (Int64, Int64, Int64)
  deriving (Eq, Show, Ord)

solve :: C.ByteString -> Either String (Int, Int64)
solve content = Right (solve' (inputParser content) 1000)

inputParser :: C.ByteString -> [Point]
inputParser input = mapMaybe parsePoint (C.split '\n' input)
  where
    parsePoint bs = case map (fmap fst . C.readInt64) (C.split ',' bs) of
      [Just x, Just y, Just z] -> Just (Point (x, y, z))
      _ -> Nothing

solve' :: [Point] -> Int -> (Int, Int64)
solve' input k =
  let sorted = sortByDist input
      ds = foldl' (\ds' (p, q) -> DS.union p q ds') DS.empty (take k sorted)
      part1 = product $ take 3 (sortDesc (map length (DS.toLists ds)))

      allDisjoint = foldl' (flip DS.insert) DS.empty input :: DS.DisjointSet Point
      initial = (allDisjoint, Point (0, 0, 0), Point (0, 0, 0))
      folded = scanl (\(ds', _, _) (p, q) -> (DS.union p q ds', p, q)) initial sorted
      folded' = map (\(ds', p, q) -> (DS.sets ds', p, q)) folded
      (_, Point (x, _, _), Point (x', _, _)) = head $ filter (\(ds', _, _) -> ds' == 1) folded'
   in (part1, x * x')

distSquared :: Point -> Point -> Int64
distSquared (Point (x, y, z)) (Point (x', y', z')) =
  let dx = x' - x
      dy = y' - y
      dz = z' - z
   in dx * dx + dy * dy + dz * dz

sortByDist :: [Point] -> [(Point, Point)]
sortByDist points =
  let candidates = choose2 points
      candidatesWithDist = map (\(p, q) -> (distSquared p q, p, q)) candidates
      sorted = sort candidatesWithDist
   in map (\(_, p, q) -> (p, q)) sorted
