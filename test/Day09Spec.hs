module Day09Spec (spec) where

import qualified Data.ByteString as C
import Day09 (solve)
import Protolude
import System.Directory (doesFileExist)
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve the example" $ do
      solve exampleInput `shouldBe` Right (50, 24)
    it "should solve the actual problem" $ do
      maybeInput <- readMyInput "input/09.txt"
      case maybeInput of
        Nothing -> pendingWith "input file missing"
        Just input' -> solve input' `shouldBe` Right (4750176210, 1574684850)
  where
    exampleInput =
      """
      7,1
      11,1
      11,7
      9,7
      9,5
      2,5
      2,3
      7,3

      """

readMyInput :: FilePath -> IO (Maybe ByteString)
readMyInput fp = do
  exists <- doesFileExist fp
  if exists then Just <$> C.readFile fp else return Nothing
