module Day10Spec (spec) where

import Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.ByteString.Char8 as BS
import Data.Either (fromRight)
import qualified Data.Sequence as Seq
import Day10
import Test.Hspec

spec :: Spec
spec = do
  describe "parseLights" $ do
    it "should parse the lights from left-to-right" $ do
      parseLights "#....." `shouldBe` Lights {bits = 1, size = 6}
      parseLights ".#...." `shouldBe` Lights {bits = 2, size = 6}

  describe "toggleBits" $ do
    it "should toggle the bits" $ do
      toggleBits (bits (parseLights "#.....")) [0, 1, 5] `shouldBe` bits (parseLights ".#...#")
      toggleBits (bits (parseLights "#.....")) [0, 3, 4] `shouldBe` bits (parseLights "...##.")

  describe "parseInput" $ do
    it "should parse the example correctly" $ do
      let tcs = fromRight [] $ parseOnly tcParser exampleInput
      length tcs `shouldBe` 3
      let tc1 : tc2 : tc3 : _ = tcs
      tc1 `shouldBe` (parseLights ".##.", Seq.fromList [[3], [1, 3], [2], [2, 3], [0, 2], [0, 1]], [3, 5, 4, 7])
      tc2 `shouldBe` (parseLights "...#.", Seq.fromList [[0, 2, 3, 4], [2, 3], [0, 4], [0, 1, 2], [1, 2, 3, 4]], [7, 5, 12, 7, 2])
      tc3 `shouldBe` (parseLights ".###.#", Seq.fromList [[0, 1, 2, 3, 4], [0, 3, 4], [0, 1, 2, 4, 5], [1, 2]], [10, 11, 11, 5, 10, 5])

  describe "solve" $ do
    it "should solve the example" $ do
      solve exampleInput `shouldBe` Right (7, 0)

exampleInput :: BS.ByteString
exampleInput =
  """
  [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
  [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
  [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}

  """
