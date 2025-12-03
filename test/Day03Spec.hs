module Day03Spec (spec) where

import Day03 (solve)
import Test.Hspec
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve the example" $ do
      solve exampleInput `shouldBe` Right (357, 0)

exampleInput :: T.Text
exampleInput =
  """
  987654321111111
  811111111111119
  234234234234278
  818181911112111 

  """
