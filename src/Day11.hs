module Day11 (solve) where

import Data.Attoparsec.ByteString.Char8 (Parser, char, endOfLine, isSpace, parseOnly, sepBy, sepBy1, skipSpace, takeTill)
import qualified Data.ByteString.Char8 as C
import Data.Graph (Graph, Vertex, graphFromEdges)
import Protolude hiding (isSpace)
import Data.MemoTrie
import Data.Array ((!))

type Label = C.ByteString

type Targets = [C.ByteString]

type AocInput = [(Label, Targets)]

solve :: C.ByteString -> Either Text (Maybe Int64, Maybe Int64)
solve content = solve' <$> first toS (parseOnly inputParser content)

inputParser :: Parser AocInput
inputParser = lineParser `sepBy` endOfLine

lineParser :: Parser (Label, Targets)
lineParser =
  (,)
    <$> takeTill (== ':')
    <* char ':'
    <* skipSpace
    <*> sepBy1 (takeTill isSpace) (char ' ' >> skipSpace)

buildGraph :: AocInput -> (Graph, Vertex -> (Label, Label, Targets), Label -> Maybe Vertex)
buildGraph input = graphFromEdges (map (\(x, xs) -> (x, x, xs)) (("out", []) : input))

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
countPaths graph start goal = go start
  where
    go :: Vertex -> Int64
    go = memoFix $ \rec current ->
      if current == goal then 1
      else sum [rec neighbor | neighbor <- graph ! current]
