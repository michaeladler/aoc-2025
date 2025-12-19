module Day02Spec (spec) where

import qualified Data.ByteString.Char8 as C
import Day02
import Protolude
import System.Directory (doesFileExist)
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve the example" $ do
      let exampleInput = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
      solve exampleInput `shouldBe` Right (1227775554, 4174379265)

    it "should solve the actual problem" $ do
      maybeInput <- readMyInput "input/02.txt"
      case maybeInput of
        Nothing -> pendingWith "input file missing"
        Just input' -> solve input' `shouldBe` Right (12850231731, 24774350322)

  describe "isInvalidID" $ do
    it "should detect ID 11 as invalid" $ do
      isInvalidID 11 `shouldBe` True
    it "should detect ID 22 as invalid" $ do
      isInvalidID 22 `shouldBe` True
    it "should detect ID 99 as invalid" $ do
      isInvalidID 99 `shouldBe` True
    it "should detect ID 100 as valid" $ do
      isInvalidID 100 `shouldBe` False
    it "should detect ID 1010 as invalid" $ do
      isInvalidID 1010 `shouldBe` True
    it "should detect ID 1188511885 as invalid" $ do
      isInvalidID 1188511885 `shouldBe` True
    it "should detect ID 222222 as invalid" $ do
      isInvalidID 222222 `shouldBe` True
    it "should detect ID 446446 as invalid" $ do
      isInvalidID 446446 `shouldBe` True
    it "should detect ID 38593859 as invalid" $ do
      isInvalidID 38593859 `shouldBe` True

  describe "invalidIDs" $ do
    it "finds invalid IDs in range (11, 22)" $ do
      invalidIDs (11, 22) `shouldBe` [11, 22]

    it "finds invalid ID 99 in range (95, 115)" $ do
      invalidIDs (95, 115) `shouldBe` [99]

    it "finds invalid ID 1010 in range (998, 1012)" $ do
      invalidIDs (998, 1012) `shouldBe` [1010]

    it "finds invalid ID 1188511885 in range (1188511880, 1188511890)" $ do
      invalidIDs (1188511880, 1188511890) `shouldBe` [1188511885]

    it "finds invalid ID 222222 in range (222220, 222224)" $ do
      invalidIDs (222220, 222224) `shouldBe` [222222]

    it "finds no invalid IDs in range (1698522, 1698528)" $ do
      invalidIDs (1698522, 1698528) `shouldBe` []

    it "finds invalid ID 446446 in range (446443, 446449)" $ do
      invalidIDs (446443, 446449) `shouldBe` [446446]

    it "finds invalid ID 38593859 in range (38593856, 38593862)" $ do
      invalidIDs (38593856, 38593862) `shouldBe` [38593859]

readMyInput :: FilePath -> IO (Maybe ByteString)
readMyInput fp = do
  exists <- doesFileExist fp
  if exists then Just <$> C.readFile fp else return Nothing
