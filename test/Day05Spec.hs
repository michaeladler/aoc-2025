module Day05Spec (spec) where

import qualified Data.ByteString.Char8 as C
import Day05 (solve)
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve the example" $ do
      solve exampleInput `shouldBe` Right (3, 14)

exampleInput :: C.ByteString
exampleInput =
  """
  3-5
  10-14
  16-20
  12-18

  1
  5
  8
  11
  17
  32

  """
