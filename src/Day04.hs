module Day04 where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, endOfLine, many1, sepBy)
import qualified Data.Attoparsec.Text as AP
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Text (Text)

type MyInt = Int

data Cell = Empty | Paper
  deriving (Eq, Show)

type Grid = HashSet (Int, Int) -- first int is row, second is col

solve :: Text -> Either String (MyInt, MyInt)
solve input = case parseInput input of
  Left err -> Left err
  Right result -> Right (solveInternal result)

parseInput :: Text -> Either String [[Cell]]
parseInput = AP.parseOnly parseInput'

parseInput' :: Parser [[Cell]]
parseInput' = many1 parseCell `sepBy` endOfLine

parseCell :: Parser Cell
parseCell = (AP.char '.' >> pure Empty) <|> (AP.char '@' >> pure Paper)

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
adjacentPositions grid (row, col) =
  length $
    filter
      id
      ( map
          (`HS.member` grid)
          [ (row - 1, col - 1),
            (row - 1, col),
            (row - 1, col + 1),
            (row, col - 1),
            (row, col + 1),
            (row + 1, col - 1),
            (row + 1, col),
            (row + 1, col + 1)
          ]
      )

cellsToGrid :: [[Cell]] -> Grid
cellsToGrid cells =
  let zipped = zip (map (\ys -> zip ys [1 ..]) cells) [1 ..] :: [([(Cell, Int)], Int)] -- first int is col, second int is row
      tmp = concatMap f zipped
      tmp' = filter (\(_, c) -> isPaper c) tmp
   in HS.fromList $ map fst tmp'
  where
    f (xs, row) = map (\(cell, col) -> ((row, col), cell)) xs

isPaper :: Cell -> Bool
isPaper Paper = True
isPaper _ = False

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
