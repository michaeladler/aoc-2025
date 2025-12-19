module Day01Spec (spec) where

import qualified Data.ByteString.Char8 as C
import Day01 (solve)
import Protolude
import System.Directory (doesFileExist)
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve the example" $ do
      solve exampleInput `shouldBe` Right (3, 6)
    it "should solve the actual problem" $ do
      maybeInput <- readMyInput "input/01.txt"
      case maybeInput of
        Nothing -> pendingWith "input file missing"
        Just input' -> solve input' `shouldBe` Right (1055, 6386)

readMyInput :: FilePath -> IO (Maybe ByteString)
readMyInput fp = do
  exists <- doesFileExist fp
  if exists then Just <$> C.readFile fp else return Nothing

exampleInput :: ByteString
exampleInput =
  """
  L68
  L30
  R48
  L5
  R60
  L55
  L1
  L99
  R14
  L82

  """
