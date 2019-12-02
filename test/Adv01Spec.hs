module Adv01Spec where

import Adv01 (calculate, calculateR)
import Test.Hspec

spec :: Spec
spec = do
  describe "Adv01" $ do
    it "task 1: non recursive case" $ do
      calculate 12     `shouldBe` 2
      calculate 14     `shouldBe` 2
      calculate 1969   `shouldBe` 654
      calculate 100756 `shouldBe` 33583
    it "task 2: recursive case" $ do
      calculateR 14     `shouldBe` 2
      calculateR 1969   `shouldBe` 966
      calculateR 100756 `shouldBe` 50346
