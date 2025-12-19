module Day03Spec (spec) where

import qualified Data.ByteString.Char8 as C
import Day03 (solve)
import Protolude
import System.Directory (doesFileExist)
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve the example" $ do
      solve exampleInput `shouldBe` Right (357, 3121910778619)
    it "should solve the actual problem" $ do
      maybeInput <- readMyInput "input/03.txt"
      case maybeInput of
        Nothing -> pendingWith "input file missing"
        Just input' -> solve input' `shouldBe` Right (16946, 168627047606506)

readMyInput :: FilePath -> IO (Maybe ByteString)
readMyInput fp = do
  exists <- doesFileExist fp
  if exists then Just <$> C.readFile fp else return Nothing

exampleInput :: C.ByteString
exampleInput =
  """
  987654321111111
  811111111111119
  234234234234278
  818181911112111 

  """
