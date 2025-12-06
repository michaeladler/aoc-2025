module Day01Spec (spec) where

import Day01 (solve)
import Test.Hspec
import qualified Data.ByteString.Char8 as C

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve the example" $ do
      solve exampleInput `shouldBe` Right (3, 6)

exampleInput :: C.ByteString
exampleInput =
  """
  L68
  L30
  R48
  L5
  R60
  L55
  L1
  L99
  R14
  L82

  """
