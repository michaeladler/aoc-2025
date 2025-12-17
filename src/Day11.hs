module Day11 where

import Data.Attoparsec.ByteString.Char8 (Parser, char, endOfLine, isSpace, parseOnly, sepBy, sepBy1, skipSpace, takeTill)
import qualified Data.ByteString.Char8 as C
import Data.Either (fromRight)
import Data.Graph (Graph, Vertex, graphFromEdges)
import Data.Array

type MyInt = Int

type Label = C.ByteString

type Targets = [C.ByteString]

type AocInput = [(Label, Targets)]

solve :: C.ByteString -> Either String (MyInt, MyInt)
solve content = case parseOnly inputParser content of
  Left err -> Left err
  Right result -> Right (solve' result)

inputParser :: Parser AocInput
inputParser = lineParser `sepBy` endOfLine

lineParser :: Parser (Label, Targets)
lineParser = do
  label <- takeTill (== ':')
  _ <- char ':'
  skipSpace
  targets <- sepBy1 (takeTill isSpace) (char ' ' >> skipSpace)
  pure (label, targets)

buildGraph :: AocInput -> (Graph, Vertex -> (Label, Label, Targets), Label -> Maybe Vertex)
buildGraph input = 
    let input' = ("out", []) : input in 
    graphFromEdges (map (\(x, xs) -> (x, x, xs)) input')

solve' :: AocInput -> (MyInt, MyInt)
solve' input =
  let (graph, _nodeFromVertex, vertexFromKey) = buildGraph input
      (you, out) = case (vertexFromKey "you", vertexFromKey "out") of
        (Just you', Just out') -> (you', out')
        _ -> error "\"you\" and/or \"out\" not found"
      part1 = length (allSimplePaths graph you out)
   in (part1, 0)

allSimplePaths :: Graph -> Vertex -> Vertex -> [[Vertex]]
allSimplePaths graph start goal = go start []
  where
    neighbors curr = graph ! curr
    go curr path
      | curr == goal = [reverse (curr : path)]
      | otherwise =
          concat
            [ go next (curr : path)
            | next <- neighbors curr,
              next `notElem` path -- Avoid cycles in this path
            ]

-- Experimental Area
example :: AocInput
example = fromRight [] $ parseOnly inputParser exampleInput

exampleInput :: C.ByteString
exampleInput =
  """
  aaa: you hhh
  you: bbb ccc
  bbb: ddd eee
  ccc: ddd eee fff
  ddd: ggg
  eee: out
  fff: out
  ggg: out
  hhh: ccc fff iii
  iii: out

  """
