module Day04 where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, endOfLine, many1, sepBy)
import qualified Data.Attoparsec.Text as AP
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)
import Data.Text (Text)

type MyInt = Int

data Cell = Paper | Empty
  deriving (Eq)

instance Show Cell where
  show Paper = "@"
  show Empty = "."

type Grid = HM.HashMap (Int, Int) Cell -- first int is row, second is col

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
      canBeRemoved k = length (filter (== Paper) (adjacentPositions grid k)) < 4
      part1 = HM.foldlWithKey' f 0 grid
      f acc k cell = case cell of
        Empty -> acc
        Paper -> if canBeRemoved k then acc + 1 else acc
   in (part1, 0)

-- collect neighbors from all adjacent positions (up to 8)
adjacentPositions :: Grid -> (Int, Int) -> [Cell]
adjacentPositions grid (row, col) =
  mapMaybe
    (`HM.lookup` grid)
    [ (row - 1, col - 1),
      (row - 1, col),
      (row - 1, col + 1),
      (row, col - 1),
      (row, col + 1),
      (row + 1, col - 1),
      (row + 1, col),
      (row + 1, col + 1)
    ]

-- Experimental Area

cellsToGrid :: [[Cell]] -> Grid
cellsToGrid cells =
  let zipped = zip (map (\ys -> zip ys [1 ..]) cells) [1 ..] :: [([(Cell, Int)], Int)] -- first int is col, second int is row
   in HM.fromList $ concatMap f zipped
  where
    f (xs, row) = map (\(cell, col) -> ((row, col), cell)) xs

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
