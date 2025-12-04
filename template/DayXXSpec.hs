module DayXXSpec (spec) where

import qualified Data.Text as T
import DayXX (solve)
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve the example" $ do
      solve exampleInput `shouldBe` Right (0, 0)

exampleInput :: T.Text
exampleInput =
  """
  987654321111111
  811111111111119
  234234234234278
  818181911112111 

  """
