module AocUtilsSpec (spec) where

import Protolude
import Test.Hspec
import qualified Data.Sequence as Seq
import qualified Data.IntMap as IntMap
import AocUtils

spec :: Spec
spec = describe "columns" $ do
  it "returns empty map for empty input" $
    columns ([] :: [[Int]]) `shouldBe` IntMap.empty

  it "returns columns for a single row" $
    columns [[1,2,3]] `shouldBe`
      IntMap.fromList
        [ (0, Seq.fromList [1])
        , (1, Seq.fromList [2])
        , (2, Seq.fromList [3])
        ]

  it "returns columns for multiple rows" $
    columns [[1,2,3],[4,5,6]] `shouldBe`
      IntMap.fromList
        [ (0, Seq.fromList [1,4])
        , (1, Seq.fromList [2,5])
        , (2, Seq.fromList [3,6])
        ]

  it "handles non-square input (ragged rows)" $
    columns [[1,2],[3,4,5]] `shouldBe`
      IntMap.fromList
        [ (0, Seq.fromList [1,3])
        , (1, Seq.fromList [2,4])
        , (2, Seq.fromList [5])
        ]
