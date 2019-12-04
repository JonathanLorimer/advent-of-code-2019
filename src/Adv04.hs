module Adv04 where

import System.IO
import Data.List (lines)
import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Map as M
import Data.Maybe
import Control.Applicative (liftA2)

range :: [Int]
range = [178416..676461]

-- Helpers

digits :: Int -> [Int]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

-- Rules

ascending :: Int -> Bool
ascending = go . digits
  where
    go (x:y:xs) = x <= y && go (y:xs)
    go _ = True

adjacent :: Int -> Bool
adjacent = isJust . L.find ((>= 2) . length) . L.group . digits

adjacent' :: Int -> Bool
adjacent' = isJust . L.find (( == 2) . length) . L.group . digits

solve1 :: Int
solve1 = length $ L.filter (\x -> ascending x && adjacent x) range

solve2 :: Int
solve2 = length $ L.filter (\x -> ascending x && adjacent' x) range

-- Solution 1.
-- $> solve1
-- Solution 2
-- $> solve2
