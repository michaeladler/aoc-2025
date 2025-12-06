module Day03Spec (spec) where

import qualified Data.ByteString.Char8 as C
import Day03 (solve)
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve the example" $ do
      solve exampleInput `shouldBe` Right (357, 3121910778619)

exampleInput :: C.ByteString
exampleInput =
  """
  987654321111111
  811111111111119
  234234234234278
  818181911112111 

  """
