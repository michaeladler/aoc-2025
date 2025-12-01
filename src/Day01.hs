module Day01 (solve) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Foldable (foldl')
import Data.Int (Int32)
import Data.Text (Text)

type MyInt = Int32

data Direction = L | R
  deriving (Show, Eq)

newtype Command = Command (Direction, MyInt)
  deriving (Show, Eq)

solve :: Text -> Either String (MyInt, MyInt)
solve content = do
  case parseCommands content of
    Left err -> Left err
    Right result -> Right (solveInternal result)

parseCommands :: Text -> Either String [Command]
parseCommands = parseOnly (many' commandParser)
  where
    commandParser :: Parser Command
    commandParser = do
      dir <- (char 'L' >> pure L) <|> (char 'R' >> pure R)
      d <- decimal
      endOfLine
      pure (Command (dir, d))

solveInternal :: [Command] -> (MyInt, MyInt)
solveInternal commands = (part1, part2)
  where
    (_, part1, part2) = foldl' step (50, 0, 0) commands
    step (pos, p1, p2) (Command (dir, delta)) =
      let binOp = if dir == L then (-) else (+)
          newPos = pos `binOp` delta
          r = newPos `mod` 100
          q' = countCrossings pos delta binOp
       in (r, if r == 0 then p1 + 1 else p1, p2 + q')

    countCrossings pos delta binOp = countCrossings' pos delta binOp 0
    countCrossings' pos delta binOp cnt =
      if delta == 0
        then cnt
        else
          ( let newPos = pos `binOp` 1
                r = newPos `mod` 100
             in countCrossings' newPos (delta - 1) binOp (if r == 0 then cnt + 1 else cnt)
          )
