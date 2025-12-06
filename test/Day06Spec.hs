module Day06Spec (spec) where

import Day06 (solve)
import qualified Data.ByteString.Char8 as C
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve the example" $ do
      solve exampleInput `shouldBe` Right (4277556, 3263827)

exampleInput :: C.ByteString
exampleInput =
  """
  123 328  51 64 
   45 64  387 23 
    6 98  215 314
  *   +   *   +  

  """
