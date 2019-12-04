module Adv03 where

import System.IO
import Data.List (lines)
import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Map as M
import Data.Maybe
import Control.Applicative (liftA2)


data Direction = L | R | U | D deriving (Eq, Ord, Show, Read)

main :: IO ()
main = do
  handle <- openFile "../inputs/03.txt" ReadMode
  file <- hGetContents handle
  let [w1, w2] = cMap 0 (0,0) . parse <$> lines file
  let m = w1 `intersection` w2
  let coords = keys . delete (0,0) $ m
  -- Solution 1.
  print $ L.minimum . fmap (manhattan (0,0)) $ coords
  -- Solution 2.
  print $ L.minimum . catMaybes $ nearest w1 w2 <$> coords
  hClose handle

nearest :: Map (Int, Int) Length -> Map (Int, Int) Length -> (Int, Int) -> Maybe Length
nearest m1 m2 k = liftA2 (+) (M.lookup k m1) (M.lookup k m2)

type Length = Int

coordinates :: (Int, Int) -> Direction -> Int -> Length -> [((Int, Int), Length)]
coordinates (x,y) L t l =
  [ ((x', y ), l + d) | (x',d) <- L.zip [x, x-1 .. x - t] [0..] ]
coordinates (x,y) R t l =
  [ ((x', y ), l + d) | (x',d) <- L.zip [x .. x + t] [0..] ]
coordinates (x,y) U t l =
  [ ((x , y'), l + d) | (y',d) <- L.zip [y .. y + t] [0..] ]
coordinates (x,y) D t l =
  [ ((x , y'), l + d) | (y',d) <- L.zip [y, y-1 .. y - t] [0..] ]

cMap :: Length -> (Int, Int) -> [(Direction, Int)] -> Map (Int, Int) Int
cMap l o [] = mempty
cMap l o ((d, t):xs) =
  fromList (coordinates o d t l)
    `union` case d of
              L -> cMap (l + t) (fst o - t, snd o) xs
              R -> cMap (l + t) (fst o + t, snd o) xs
              D -> cMap (l + t) (fst o, snd o - t) xs
              U -> cMap (l + t) (fst o, snd o + t) xs

parse :: String -> [(Direction, Int)]
parse = fmap (\(x:xs) -> (read (pure x) :: Direction, read xs :: Int)) . splitOn ","

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')
