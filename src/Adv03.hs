module Adv03 where

import System.IO
import Data.List (lines)
import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Set
import Data.Maybe
import Control.Applicative (liftA2)


data Direction = L | R | U | D deriving (Eq, Ord, Show, Read)

main :: IO ()
main = do
  handle <- openFile "../inputs/03.txt" ReadMode
  file <- hGetContents handle
  let [w1, w2] = splitOn "," <$> lines file
  let coords = delete (0,0) $ mkSet w1 `intersection` mkSet w2
  print $ manhattan (0,0) $ nearest (0,0) coords
  print $ findLeast (mkList w1) (mkList w2) coords
  hClose handle


mkSet = cSet (0,0) . parse
mkList = L.nub . cList (0,0) . parse


findLeast :: [(Int, Int)] -> [(Int, Int)] -> Set (Int, Int) -> [Int]
findLeast d1 d2 coords =
  let go l = flip L.elemIndex l <$> toList coords
   in catMaybes $ zipWith (liftA2 (+)) (go d1) (go d2)

coordinates :: (Int, Int) -> Direction -> Int -> [(Int, Int)]
coordinates (x,y) L t = [ (x', y ) | x' <- [x - t .. x] ]
coordinates (x,y) R t = [ (x', y ) | x' <- [x .. x + t] ]
coordinates (x,y) U t = [ (x , y') | y' <- [y .. y + t] ]
coordinates (x,y) D t = [ (x , y') | y' <- [y - t .. y] ]

cSet :: (Int, Int) -> [(Direction, Int)] -> Set (Int, Int)
cSet o [] = empty
cSet o ((d, t):xs) =
  fromList (coordinates o d t)
    `union` case d of
              L -> cSet (fst o - t, snd o) xs
              R -> cSet (fst o + t, snd o) xs
              D -> cSet (fst o, snd o - t) xs
              U -> cSet (fst o, snd o + t) xs



cList :: (Int, Int) -> [(Direction, Int)] -> [(Int, Int)]
cList o [] = []
cList o ((d, t):xs) = let c = coordinates o d t
  in
  (if head c == o
     then c
     else reverse c)
  ++ case d of
        L -> cList (fst o - t, snd o) xs
        R -> cList (fst o + t, snd o) xs
        D -> cList (fst o, snd o - t) xs
        U -> cList (fst o, snd o + t) xs


parse :: [String] -> [(Direction, Int)]
parse = fmap (\(x:xs) -> (read (pure x) :: Direction, read xs :: Int))

nearest :: (Int, Int) -> Set (Int, Int) -> (Int, Int)
nearest o s = foldl'
  (\z c -> if manhattan o c < manhattan o z then c else z)
  (elemAt 0 s)
  s

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')

w1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
w2 = "U62,R66,U55,R34,D71,R55,D58,R83"

p1 = mkSet . splitOn "," $ w1
p2 = mkSet . splitOn "," $ w2

l1 = mkList $ splitOn "," w1
l2 = mkList $ splitOn "," w2

coords = delete (0,0) $ intersection p1 p2

-- $> read "D" :: Direction

-- $> nearest (0,0) $ delete (0,0) $ intersection p1 p2

-- $> coords
--
-- $> findLeast l1 l2 coords
