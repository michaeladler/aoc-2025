module Day08 where

import AocUtils (choose2, sortDesc)
import qualified Data.ByteString.Char8 as C
import qualified Data.DisjointSet as DS
import Data.Int (Int32, Int64)
import Data.List (sort)
import Data.Maybe (mapMaybe)

newtype Point = Point (Int64, Int64, Int64)
  deriving (Eq, Show, Ord)

-- part1: 9600 is too low
solve :: C.ByteString -> Either String (Int, Int32)
solve content = Right (solve' (inputParser content) 1000)

inputParser :: C.ByteString -> [Point]
inputParser input = mapMaybe parsePoint (C.split '\n' input)
  where
    parsePoint bs = case map (fmap fst . C.readInt64) (C.split ',' bs) of
      [Just x, Just y, Just z] -> Just (Point (x, y, z))
      _ -> Nothing

solve' :: [Point] -> Int -> (Int, Int32)
solve' input k =
  let shortest = take k $ sortByDist input
      ds = foldl' (\ds' (x, y) -> DS.union x y ds') DS.empty shortest
      part1 = product $ take 3 (sortDesc (map length (DS.toLists ds)))
   in (part1, 0)

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

-- Experimental Area
example :: [Point]
example = inputParser exampleInput

exampleInput :: C.ByteString
exampleInput =
  """
  162,817,812
  57,618,57
  906,360,560
  592,479,940
  352,342,300
  466,668,158
  542,29,236
  431,825,988
  739,650,466
  52,470,668
  216,146,977
  819,987,18
  117,168,530
  805,96,715
  346,949,466
  970,615,88
  941,993,340
  862,61,35
  984,92,344
  425,690,689

  """
