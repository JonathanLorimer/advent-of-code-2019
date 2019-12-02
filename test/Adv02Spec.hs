module Adv02Spec where

import Adv02
import Test.Hspec hiding (focus)
import Data.Array as A
import Control.Monad

spec :: Spec
spec = do
  describe "Zipper semantics" $ do
    it "fromList" $ do
      let l = A.listArray (0,-1) []
      let r = A.listArray (0,3) [2, 3, 4, 5]
      fromList [1,2,3,4,5]
        `shouldBe` Zipper l 1 r
    it "fromList" $ do
      let l = A.listArray (0,-1) []
      let r = A.listArray (0,3) [2, 3, 4, 5]
      let l' = shiftR (fromList [1,2,3,4,5,6,7,8,9,10]) 5
      toList (Zipper l 1 r)
        `shouldBe` [1,2,3,4,5]
      toList l'
        `shouldBe` [1,2,3,4,5,6,7,8,9,10]
    it "focus" $ do
      let l = A.listArray (0,1) [1,2]
      let r = A.listArray (0,1) [4,5]
      focus (Zipper l 3 r)
          `shouldBe` 3
    it "right" $ do
      let l = iterate right $ fromList [1,2,3,4,5]
      focus (l !! 0) `shouldBe` 1
      focus (l !! 1) `shouldBe` 2
      focus (l !! 2) `shouldBe` 3
      focus (l !! 3) `shouldBe` 4
      focus (l !! 4) `shouldBe` 5
      focus (l !! 5) `shouldBe` 5
    it "left" $ do
      let l = iterate left
            $ shiftR (fromList [1,2,3,4,5]) 5
      focus (l !! 0) `shouldBe` 5
      focus (l !! 1) `shouldBe` 4
      focus (l !! 2) `shouldBe` 3
      focus (l !! 3) `shouldBe` 2
      focus (l !! 4) `shouldBe` 1
      focus (l !! 5) `shouldBe` 1
    it "%" $ do
      let l = fromList [1,2,3,4,5]
      l % 0 `shouldBe` 1
      l % 1 `shouldBe` 2
      l % 2 `shouldBe` 3
      l % 3 `shouldBe` 4
      l % 4 `shouldBe` 5
      let l' = shiftR (fromList [1,2,3,4,5,6,7,8,9,10]) 5
      l' % 0 `shouldBe` 1
      l' % 1 `shouldBe` 2
      l' % 2 `shouldBe` 3
      l' % 3 `shouldBe` 4
      l' % 4 `shouldBe` 5
      l' % 5 `shouldBe` 6
      l' % 6 `shouldBe` 7
      l' % 7 `shouldBe` 8
      l' % 8 `shouldBe` 9
      l' % 9 `shouldBe` 10
    describe "update" $ do
      it "leftbound list" $ do
        let l = fromList [1,2,3,4,5]
        update l 0 6 % 0 `shouldBe` 6
        update l 1 6 % 1 `shouldBe` 6
        update l 2 6 % 2 `shouldBe` 6
        update l 3 6 % 3 `shouldBe` 6
        update l 4 6 % 4 `shouldBe` 6
      it "centerbound list" $ do
        let l' = shiftR (fromList [1,2,3,4,5,6,7,8,9,10]) 5
        update l' 0 6 % 0 `shouldBe` 6
        update l' 1 6 % 1 `shouldBe` 6
        update l' 2 6 % 2 `shouldBe` 6
        update l' 3 6 % 3 `shouldBe` 6
        update l' 4 6 % 4 `shouldBe` 6
        update l' 7 6 % 7 `shouldBe` 6
        update l' 8 6 % 8 `shouldBe` 6
        update l' 9 6 % 9 `shouldBe` 6
      it "rightbound list" $ do
        let l'' = shiftR (fromList [1,2,3,4,5,6,7,8,9,10]) 9
        update l'' 0 99 % 0 `shouldBe` 99
        update l'' 1 99 % 1 `shouldBe` 99
        update l'' 2 66 % 2 `shouldBe` 66
        update l'' 3 66 % 3 `shouldBe` 66
        update l'' 4 66 % 4 `shouldBe` 66
        update l'' 7 66 % 7 `shouldBe` 66
        update l'' 8 66 % 8 `shouldBe` 66
        update l'' 9 66 % 9 `shouldBe` 66
  describe "1202 Program Alert" $ do
    describe "task 1: working computer" $ do
      let t = toList . compute . fromList
      it "[1,0,0,0,99]" $
        t [1,0,0,0,99]   `shouldBe` [2,0,0,0,99]
      it "[2,3,0,3,99]" $
        t [2,3,0,3,99]   `shouldBe` [2,3,0,6,99]
      it "[2,4,4,5,99,0]" $
        t [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]
      it "[1,1,1,4,99,5,6,0,99]" $
        t [1,1,1,4,99,5,6,0,99]
          `shouldBe` [30,1,1,4,2,5,6,0,99]
          {-
    it "task 2: recursive case" $ do
      calculateR 14     `shouldBe` 2
      calculateR 1969   `shouldBe` 966
      calculateR 100756 `shouldBe` 50346
      -}
