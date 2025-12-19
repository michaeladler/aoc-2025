module Day06Spec (spec) where

import qualified Data.ByteString.Char8 as C
import Day06 (solve)
import Protolude
import System.Directory (doesFileExist)
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve the example" $ do
      solve exampleInput `shouldBe` Right (4277556, Just 3263827)
    it "should solve the actual problem" $ do
      maybeInput <- readMyInput "input/06.txt"
      case maybeInput of
        Nothing -> pendingWith "input file missing"
        Just input' -> solve input' `shouldBe` Right (4693419406682, Just 9029931401920)

readMyInput :: FilePath -> IO (Maybe ByteString)
readMyInput fp = do
  exists <- doesFileExist fp
  if exists then Just <$> C.readFile fp else return Nothing

exampleInput :: C.ByteString
exampleInput =
  """
  123 328  51 64 
   45 64  387 23 
    6 98  215 314
  *   +   *   +  

  """
