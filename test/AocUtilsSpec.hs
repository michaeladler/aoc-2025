module AocUtilsSpec (spec) where

import AocUtils
import qualified Data.ByteString as BS
import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import Protolude
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "columns" $ do
  it "returns empty map for empty input" $
    columns ([] :: [[Int]]) `shouldBe` IntMap.empty

  it "returns columns for a single row" $
    columns [[1, 2, 3]]
      `shouldBe` IntMap.fromList
        [ (0, Seq.fromList [1]),
          (1, Seq.fromList [2]),
          (2, Seq.fromList [3])
        ]

  it "returns columns for multiple rows" $
    columns [[1, 2, 3], [4, 5, 6]]
      `shouldBe` IntMap.fromList
        [ (0, Seq.fromList [1, 4]),
          (1, Seq.fromList [2, 5]),
          (2, Seq.fromList [3, 6])
        ]

  it "handles non-square input (ragged rows)" $
    columns [[1, 2], [3, 4, 5]]
      `shouldBe` IntMap.fromList
        [ (0, Seq.fromList [1, 3]),
          (1, Seq.fromList [2, 4]),
          (2, Seq.fromList [5])
        ]

  describe "replicateInt" $ do
    it "should replicate 1000 twice to 10001000" $
      replicateInt 1000 2 `shouldBe` 10001000

  describe "numDigits" $ do
    it "should return 4 for 1000" $
      numDigits 1000 `shouldBe` 4
    it "should compute the correct number of digits" $
      property $
        \x -> numDigits x `shouldBe` BS.length (show (abs x))
