module Day04Spec (spec) where

import qualified Data.ByteString.Char8 as C
import Day04 (solve)
import Protolude
import System.Directory (doesFileExist)
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve the example" $ do
      solve exampleInput `shouldBe` Right (13, 43)
    it "should solve the actual problem" $ do
      maybeInput <- readMyInput "input/04.txt"
      case maybeInput of
        Nothing -> pendingWith "input file missing"
        Just input' -> solve input' `shouldBe` Right (1553, 8442)

readMyInput :: FilePath -> IO (Maybe ByteString)
readMyInput fp = do
  exists <- doesFileExist fp
  if exists then Just <$> C.readFile fp else return Nothing

exampleInput :: C.ByteString
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
