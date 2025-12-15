module Day10 (solve, Lights (..), parseLights, tcParser, toggleBits) where

import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfLine, many', parseOnly, sepBy, skipSpace, takeWhile1)
import Data.Bits (shiftL, testBit, xor, (.|.))
import qualified Data.ByteString.Char8 as BS
import Data.Either (fromRight)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Data.Word (Word16)

type Buttons = Seq [Int]

type Joltages = [Int]

type TC = (Lights, Buttons, Joltages)

data Lights = Lights {bits :: !Word16, size :: !Int}
  deriving (Eq, Ord)

instance Show Lights where
  show l = map bitChar [size l - 1, size l - 2 .. 0]
    where
      bitChar i = if testBit (bits l) i then '#' else '.'

solve :: BS.ByteString -> Either String (Int, Int)
solve input = case parseOnly tcParser input of
  Left err -> Left err
  Right tcs -> Right $ solve' tcs

solve' :: [TC] -> (Int, Int)
solve' tcs = (sum (map bfsPart1 tcs), 0)

bfsPart1 :: TC -> Int
bfsPart1 (target, buttons, _) = go (Seq.singleton (0, 0)) HashSet.empty
  where
    neighbors lights = fmap (toggleBits lights) buttons

    go :: Seq (Word16, Int) -> HashSet Word16 -> Int
    go Seq.Empty _ = 0 -- not found
    go ((current, depth) Seq.:<| queue) visited
      | current == bits target = depth
      | current `HashSet.member` visited = go queue visited
      | otherwise =
          let unvisitedNeighbors = Seq.filter (\x -> not (HashSet.member x visited)) (neighbors current)
              newQueue = queue >< fmap (,depth + 1) unvisitedNeighbors
              newVisited = HashSet.insert current visited
           in go newQueue newVisited

toggleBits :: Word16 -> [Int] -> Word16
toggleBits n is = let mask = foldr (\k acc -> acc .|. (1 `shiftL` k)) 0 is in n `xor` mask

tcParser :: Parser [TC]
tcParser = lineParser `sepBy` endOfLine

lineParser :: Parser TC
lineParser = (,,) <$> lightsParser <* char ' ' <*> buttonsParser <* char ' ' <*> joltageParser

buttonsParser :: Parser Buttons
buttonsParser = Seq.fromList <$> many' (skipSpace *> char '(' *> (decimal `sepBy` char ',') <* char ')')

lightsParser :: Parser Lights
lightsParser = parseLights <$> (char '[' *> takeWhile1 (\c -> c == '#' || c == '.') <* char ']')

joltageParser :: Parser Joltages
joltageParser = char '{' *> decimal `sepBy` char ',' <* char '}'

parseLights :: BS.ByteString -> Lights
parseLights bs = Lights {bits = BS.foldr (\c acc -> acc * 2 + if c == '#' then 1 else 0) 0 bs, size = BS.length bs}

-- Experimental Area
example :: [TC]
example = fromRight [] $ parseOnly tcParser exampleInput
  where
    exampleInput =
      """
      [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}

      """
