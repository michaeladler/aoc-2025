module DayXXSpec (spec) where

import qualified Data.ByteString.Char8 as C
import DayXX (solve)
import Protolude
import System.Directory (doesFileExist)
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve the example" $ do
      solve exampleInput `shouldBe` Right (0, 0)
    it "should solve the actual problem" $ do
      maybeInput <- readMyInput "input/XX.txt"
      case maybeInput of
        Nothing -> pendingWith "input file missing"
        Just input' -> solve input' `shouldBe` Right (0, 0)
  where
    exampleInput =
      """
      987654321111111
      811111111111119
      234234234234278
      818181911112111 

      """

readMyInput :: FilePath -> IO (Maybe ByteString)
readMyInput fp = do
  exists <- doesFileExist fp
  if exists then Just <$> C.readFile fp else return Nothing
