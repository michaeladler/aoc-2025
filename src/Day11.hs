module Day11 (solve) where

import Data.Array (Array, bounds, listArray, range, (!))
import Data.Attoparsec.ByteString.Char8 (Parser, char, endOfLine, isSpace, parseOnly, sepBy, sepBy1, skipSpace, takeTill)
import qualified Data.ByteString.Char8 as C
import Data.Graph (Graph, Vertex, graphFromEdges)
import Data.Int (Int64)

type Label = C.ByteString

type Targets = [C.ByteString]

type AocInput = [(Label, Targets)]

solve :: C.ByteString -> Either String (Maybe Int64, Maybe Int64)
solve content = case parseOnly inputParser content of
  Left err -> Left err
  Right result -> Right (solve' result)

inputParser :: Parser AocInput
inputParser = lineParser `sepBy` endOfLine

lineParser :: Parser (Label, Targets)
lineParser = do
  label <- takeTill (== ':')
  char ':' >> skipSpace
  targets <- sepBy1 (takeTill isSpace) (char ' ' >> skipSpace)
  pure (label, targets)

buildGraph :: AocInput -> (Graph, Vertex -> (Label, Label, Targets), Label -> Maybe Vertex)
buildGraph input =
  let input' = ("out", []) : input
   in graphFromEdges (map (\(x, xs) -> (x, x, xs)) input')

solve' :: AocInput -> (Maybe Int64, Maybe Int64)
solve' input =
  let (graph, _nodeFromVertex, vertexFromKey) = buildGraph input
      part1 = case (vertexFromKey "you", vertexFromKey "out") of
        (Just you, Just out) -> Just $ countPaths graph you out
        _ -> Nothing

      -- it turns out there are no paths from dac to fft
      -- we have to go: svr -> fft -> dac -> out
      part2 = case (vertexFromKey "svr", vertexFromKey "out", vertexFromKey "dac", vertexFromKey "fft") of
        (Just svr, Just out, Just dac, Just fft) ->
          Just $ countPaths graph svr fft * countPaths graph fft dac * countPaths graph dac out
        _ -> Nothing
   in (part1, part2)

countPaths :: Graph -> Vertex -> Vertex -> Int64
countPaths graph start goal = memo ! start
  where
    bnds = bounds graph

    -- Define the lazy cache (Array):
    -- 'memo' creates an array where every index 'v' contains the result of 'go v'
    memo :: Array Vertex Int64
    memo = listArray bnds [go v | v <- range bnds]

    -- 3. The Logic Function
    -- Instead of recursing via 'countPaths', we look up the result in 'memo'
    go :: Vertex -> Int64
    go current
      | current == goal = 1
      | otherwise = sum [memo ! neighbor | neighbor <- graph ! current]
