module Day06Spec (spec) where

import qualified Data.Text as T
import Day06 (solve)
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve the example" $ do
      solve exampleInput `shouldBe` Right (4277556, 0)

exampleInput :: T.Text
exampleInput =
  """
  123 328  51 64
   45 64  387 23
    6 98  215 314
  *   +   *   +

  """
