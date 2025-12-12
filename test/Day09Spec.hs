module Day09Spec (spec) where

import Day09 (solve)
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve the example" $ do
      solve exampleInput `shouldBe` Right (50, 0)
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
