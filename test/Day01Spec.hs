{-# LANGUAGE OverloadedStrings #-}

module Day01Spec (spec) where

import Day01 (solve)
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve the example" $ do
      solve "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82\n" `shouldBe` Right (3, 6)
