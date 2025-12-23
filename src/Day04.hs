module Day04 (solve) where

import AocUtils
import Data.Attoparsec.ByteString.Char8 (Parser, char, endOfLine, many1, sepBy)
import qualified Data.ByteString.Char8 as C
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Protolude

type MyInt = Int

data Cell = Empty | Paper
  deriving (Eq, Show)

type Grid = HashSet (Int, Int) -- first int is row, second is col

solve :: C.ByteString -> Either Text (MyInt, MyInt)
solve input = solveInternal <$> parseOnly parseInput' input

parseInput' :: Parser [[Cell]]
parseInput' = many1 parseCell `sepBy` endOfLine

parseCell :: Parser Cell
parseCell = (char '.' >> pure Empty) <|> (char '@' >> pure Paper)

solveInternal :: [[Cell]] -> (MyInt, MyInt)
solveInternal input =
  let grid = cellsToGrid input
      part1 = HS.foldl' f 0 grid
      part2 = removePaper grid
      f acc k = if canBeRemoved grid k then acc + 1 else acc
   in (part1, part2)

canBeRemoved :: Grid -> (Int, Int) -> Bool
canBeRemoved grid k = adjacentPositions grid k < 4

-- collect neighbors from all adjacent positions (up to 8)
adjacentPositions :: Grid -> (Int, Int) -> Int
adjacentPositions grid (row, col) = foldl' (\acc p -> if p `HS.member` grid then acc + 1 else acc) 0 (neighbors8 (row, col))

cellsToGrid :: [[Cell]] -> Grid
cellsToGrid cells =
  let zipped = zip (map (\ys -> zip ys [1 ..]) cells) [1 ..] :: [([(Cell, Int)], Int)] -- first int is col, second int is row
      tmp = concatMap f zipped
      tmp' = filter (\(_, c) -> c == Paper) tmp
   in HS.fromList $ map fst tmp'
  where
    f (xs, row) = map (\(cell, col) -> ((row, col), cell)) xs

-- a single iteration
removePaperStep :: Grid -> (Int, Grid)
removePaperStep grid = HS.foldl' f (0, grid) grid
  where
    f :: (Int, Grid) -> (Int, Int) -> (Int, Grid)
    f (acc, g) k = if canBeRemoved g k then (acc + 1, HS.delete k g) else (acc, g)

removePaper :: Grid -> Int
removePaper grid = go grid 0

go :: Grid -> Int -> Int
go grid count =
  let (delta, grid') = removePaperStep grid
   in if delta == 0 then count else go grid' count + delta
