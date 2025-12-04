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
      canBeRemoved k = adjacentPositions grid k < 4
      part1 = HS.foldl' f 0 grid
      f acc k = if canBeRemoved k then acc + 1 else acc
   in (part1, 0)

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

-- Experimental Area

exampleInput :: Text
exampleInput =
  """
  ..@@.@@@@.
  @@@.@.@.@@
  @@@@@.@.@@
  @.@@@@..@.
  @@.@@@@.@@
  .@@@@@@@.@
  .@.@.@.@@@
  @.@@@.@@@@
  .@@@@@@@@.
  @.@.@@@.@.

  """
