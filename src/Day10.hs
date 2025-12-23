module Day10 (solve, Lights (..), parseLights, tcParser, toggleBits) where

import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfLine, many', parseOnly, sepBy, skipSpace, takeWhile1)
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntSet as IntSet
import Data.Sequence ((><))
import qualified Data.Sequence as Seq
import qualified Data.Traversable as T
import Protolude
import System.IO.Unsafe (unsafePerformIO)
import Z3.Monad

type Buttons = Seq [Int]

type Joltages = [Int]

type TC = (Lights, Buttons, Joltages)

data Lights = Lights {bits :: !Int, size :: !Int}
  deriving (Eq, Ord, Show)

solve :: BS.ByteString -> Either Text (Int, Integer)
solve input = solve' <$> first toS (parseOnly tcParser input)

solve' :: [TC] -> (Int, Integer)
solve' tcs = (sum (map bfsPart1 tcs), sum (map part2 tcs))

bfsPart1 :: TC -> Int
bfsPart1 (target, buttons, _) = go (Seq.singleton (0, 0)) mempty
  where
    neighbors lights = fmap (toggleBits lights) buttons

    go :: Seq (Int, Int) -> IntSet -> Int
    go Seq.Empty _ = 0 -- not found
    go ((current, depth) Seq.:<| queue) visited
      | current == bits target = depth
      | current `IntSet.member` visited = go queue visited
      | otherwise =
          let unvisitedNeighbors = Seq.filter (\x -> not (IntSet.member x visited)) (neighbors current)
              newQueue = queue >< fmap (,depth + 1) unvisitedNeighbors
              newVisited = IntSet.insert current visited
           in go newQueue newVisited

toggleBits :: Int -> [Int] -> Int
toggleBits n is = let bitMask = foldr (\k acc -> acc .|. (1 `shiftL` k)) 0 is in n `xor` bitMask

part2 :: TC -> Integer
part2 tc = sum (runZ3 (script tc))

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

-- Take the first example in the sample:
-- [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
--         a    b    c    d    e      f
-- This is just a set of equations:
-- buttons = [a,b,c,d,e,f]
--           [0,1,2,3,4,5]
--
-- e + f = 3
-- b + f = 5
-- c + d + e = 4
-- a + b + d = 7
--
-- desired solution is [a,b,c,d,e,f] = [1,3,3,1,2]
--
-- and the solution is to minimize the sum of the variables, each of which must be positive.
script :: TC -> Z3 [Integer]
script (_, buttons, joltages) = do
  -- Create integer variables for each button
  buttonVars <- mapM (mkFreshIntVar . (\i -> 'b' : show i)) [0 .. (Seq.length buttons - 1)]
  let buttonVars' = Seq.fromList buttonVars
  zeroVal <- mkInteger 0

  -- Constraints: variables are positive
  optimizeAssert =<< mkAnd =<< T.sequence [mkGe v zeroVal | v <- buttonVars]

  -- For each voltage constraint
  tmp <- T.forM (zip [0 ..] joltages) $ \(i, joltage) -> do
    let vvars = [buttonVars' `Seq.index` j | (j, button) <- zip [0 ..] (toList buttons), i `elem` button]
    sumVars <- mkAdd vvars
    target <- mkInteger (fromIntegral joltage)
    mkEq sumVars target
  optimizeAssert =<< mkAnd tmp

  -- -- Minimize the sum of the button presses
  _ <- optimizeMinimize =<< mkAdd buttonVars
  model <- optimizeRun
  catMaybes <$> mapM (evalInt model) buttonVars

optimizeRun :: Z3 Model
optimizeRun = do
  optimizeCheck [] >>= \case
    Sat -> pure ()
    r -> do
      liftIO . putStrLn =<< optimizeToString
      liftIO (die (show r))
  optimizeGetModel

runZ3 :: Z3 a -> a
runZ3 = unsafePerformIO . evalZ3
