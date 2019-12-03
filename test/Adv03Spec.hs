module Adv03Spec where

import Adv03
import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Set
import Test.Hspec


t w1 w2 = manhattan (0,0)
        $ nearest (0,0)
        $ delete (0,0)
        $ (mkSet . splitOn ",") w1
          `intersection` (mkSet . splitOn ",") w2

c w1 w2 = delete (0,0)
        $ (mkSet . splitOn ",") w1
          `intersection` (mkSet . splitOn ",") w2

t' w1 w2 = findLeast (mkList $ splitOn "," w1) (mkList $ splitOn "," w2) (c w1 w2)

spec :: Spec
spec = do
  describe "03: Intersection of Wires" $ do
    it "task 1: Manhattan Distance" $ do
      t "R75,D30,R83,U83,L12,D49,R71,U7,L72"
        "U62,R66,U55,R34,D71,R55,D58,R83"
          `shouldBe` 159
      t "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
        "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
          `shouldBe` 135
    it "task 2: Steps" $ do
      t' "R75,D30,R83,U83,L12,D49,R71,U7,L72"
         "U62,R66,U55,R34,D71,R55,D58,R83"
          `shouldBe` 610
      t' "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
         "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
          `shouldBe` 410
