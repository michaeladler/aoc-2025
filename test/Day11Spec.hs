module Day11Spec (spec) where

import Day11 (solve)
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve example 1" $ do
      solve example1 `shouldBe` Right (Just 5, Nothing)
    it "should solve example 2" $ do
      solve example2 `shouldBe` Right (Nothing, Just 2)
  where
    example1 =
      """
      aaa: you hhh
      you: bbb ccc
      bbb: ddd eee
      ccc: ddd eee fff
      ddd: ggg
      eee: out
      fff: out
      ggg: out
      hhh: ccc fff iii
      iii: out

      """

    example2 =
      """
      svr: aaa bbb
      aaa: fft
      fft: ccc
      bbb: tty
      tty: ccc
      ccc: ddd eee
      ddd: hub
      hub: fff
      eee: dac
      dac: fff
      fff: ggg hhh
      ggg: out
      hhh: out
      """
