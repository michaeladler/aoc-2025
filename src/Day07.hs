module Day07 where

import qualified Data.ByteString.Char8 as C
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Maybe (isJust)

type MyInt = Int

type Point = (MyInt, MyInt) -- x, y

solve :: C.ByteString -> Either String (MyInt, MyInt)
solve input = case parseInput input of
  (Nothing, _) -> Left "Start not found"
  (Just start, splitters) -> Right (solve' start splitters)

solve' :: Point -> HashSet Point -> (MyInt, MyInt)
solve' start splitters = (solvePart1 start splitters, 0)

solvePart1 :: Point -> HashSet Point -> MyInt
solvePart1 start splitters =
  let xMax = foldl' (\x' (x, _) -> max x' x) minBound splitters
      go acc beams = case go' beams splitters xMax of
        (delta, []) -> acc + delta
        (delta, beams') -> go (acc + delta) beams'
   in go 0 [start]

isBeamDone :: (MyInt, MyInt) -> MyInt -> Bool
isBeamDone (x, _) xMax = x > xMax

go' :: [Point] -> HashSet Point -> MyInt -> (Int, [(MyInt, MyInt)])
go' beams splitters xMax =
  let (delta, beams') = advanceBeams beams splitters
      beams'' = filter (\p -> not (isBeamDone p xMax)) beams'
   in (delta, beams'')

advanceBeams :: [Point] -> HashSet Point -> (MyInt, [Point])
advanceBeams beams splitters =
  let (splitCount, hs) = foldl' step (0, HashSet.empty) beams
   in (splitCount, HashSet.toList hs)
  where
    step (acc, beams') (x, y) =
      let newBeam = (x + 1, y)
       in case HashSet.member newBeam splitters of
            True -> (acc + 1, HashSet.union (HashSet.fromList [(x + 1, y - 1), (x + 1, y + 1)]) beams')
            False -> (acc, HashSet.insert newBeam beams')

parseInput :: C.ByteString -> (Maybe Point, HashSet Point)
parseInput bs =
  let rows = zip (C.split '\n' bs) [0 ..]
      rowsParsed = map (uncurry parseRow) rows
   in foldl' (\(s, hs) (s', hs') -> (if isJust s' then s' else s, HashSet.union hs hs')) (Nothing, HashSet.empty) rowsParsed
  where
    parseRow :: C.ByteString -> MyInt -> (Maybe Point, HashSet Point)
    parseRow rowBs x = let (_, b, c) = C.foldl' parseChar (0, Nothing, HashSet.empty) rowBs in (b, c)
      where
        parseChar :: (MyInt, Maybe Point, HashSet Point) -> Char -> (MyInt, Maybe Point, HashSet Point)
        parseChar (y, start, hs) c = case c of
          'S' -> (y + 1, Just (x, y), hs)
          '^' -> (y + 1, start, HashSet.insert (x, y) hs)
          _ -> (y + 1, start, hs)

-- Experimental Area
example :: (Point, HashSet Point)
example = case parseInput exampleInput of
  (Just p, xs) -> (p, xs)
  _ -> undefined

exampleInput :: C.ByteString
exampleInput =
  """
  .......S.......
  ...............
  .......^.......
  ...............
  ......^.^......
  ...............
  .....^.^.^.....
  ...............
  ....^.^...^....
  ...............
  ...^.^...^.^...
  ...............
  ..^...^.....^..
  ...............
  .^.^.^.^.^...^.
  ...............

  """
