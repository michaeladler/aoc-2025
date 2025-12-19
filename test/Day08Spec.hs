module Day08Spec (spec) where

import qualified Data.ByteString.Char8 as C
import Day08
import Protolude
import System.Directory (doesFileExist)
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve the example" $ do
      solve' (inputParser exampleInput) 10 `shouldBe` Just (40, 25272)
    it "should solve the actual problem" $ do
      maybeInput <- readMyInput "input/08.txt"
      case maybeInput of
        Nothing -> pendingWith "input file missing"
        Just input' -> solve input' `shouldBe` Right (75680, 8995844880)
  where
    exampleInput =
      """
      162,817,812
      57,618,57
      906,360,560
      592,479,940
      352,342,300
      466,668,158
      542,29,236
      431,825,988
      739,650,466
      52,470,668
      216,146,977
      819,987,18
      117,168,530
      805,96,715
      346,949,466
      970,615,88
      941,993,340
      862,61,35
      984,92,344
      425,690,689

      """

readMyInput :: FilePath -> IO (Maybe ByteString)
readMyInput fp = do
  exists <- doesFileExist fp
  if exists then Just <$> C.readFile fp else return Nothing
