module Day05Spec (spec) where

import qualified Data.ByteString.Char8 as C
import Day05 (solve)
import Protolude
import System.Directory (doesFileExist)
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve the example" $ do
      solve exampleInput `shouldBe` Right (3, 14)
    it "should solve the actual problem" $ do
      maybeInput <- readMyInput "input/05.txt"
      case maybeInput of
        Nothing -> pendingWith "input file missing"
        Just input' -> solve input' `shouldBe` Right (737, 357485433193284)

readMyInput :: FilePath -> IO (Maybe ByteString)
readMyInput fp = do
  exists <- doesFileExist fp
  if exists then Just <$> C.readFile fp else return Nothing

exampleInput :: C.ByteString
exampleInput =
  """
  3-5
  10-14
  16-20
  12-18

  1
  5
  8
  11
  17
  32

  """
