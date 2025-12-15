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
solve input = case parseInput input of
  Left err -> Left err
  Right tcs -> Right $ solveInternal tcs

solveInternal :: [TC] -> (Int, Int)
solveInternal tcs = (sum $ map (\(lights, buttons, _) -> bfsPart1 lights buttons) tcs, 0)

bfsPart1 :: Lights -> Buttons -> Int
bfsPart1 target buttons = go (Seq.singleton (0, 0)) HashSet.empty
  where
    neighbors :: Word16 -> Seq Word16
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
toggleBits n is = n `xor` mask
  where
    mask = foldr (\k acc -> acc .|. (1 `shiftL` k)) 0 is

parseInput :: BS.ByteString -> Either String [TC]
parseInput = parseOnly tcParser

tcParser :: Parser [TC]
tcParser = lineParser `sepBy` endOfLine

lineParser :: Parser TC
lineParser = do
  lights <- lightsParser
  _ <- char ' '
  buttons <- buttonsParser
  _ <- char ' '
  joltages <- joltageParser
  pure (lights, buttons, joltages)

buttonsParser :: Parser Buttons
buttonsParser = Seq.fromList <$> many' parseGroup
  where
    parseGroup :: Parser [Int]
    parseGroup = skipSpace *> char '(' *> (decimal `sepBy` char ',') <* char ')'

lightsParser :: Parser Lights
lightsParser = do
  _ <- char '['
  bs <- takeWhile1 (\c -> c == '#' || c == '.')
  _ <- char ']'
  return (parseLights bs)

joltageParser :: Parser Joltages
joltageParser = do
  _ <- char '{'
  xs <- decimal `sepBy` char ','
  _ <- char '}'
  return xs

parseLights :: BS.ByteString -> Lights
parseLights bs =
  let n = BS.length bs
   in Lights {bits = parseBits bs, size = n}
  where
    parseBits :: BS.ByteString -> Word16
    parseBits = BS.foldr step 0
      where
        step c acc = acc * 2 + if c == '#' then 1 else 0

-- Experimental Area
example :: [TC]
example = fromRight [] $ parseOnly tcParser exampleInput
  where
    exampleInput =
      """
      [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}

      """
