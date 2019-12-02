module Adv02 where

import System.IO
import Data.List (lines, foldl')
import Data.List.Split (splitOn)
import Data.Array (Array)
import Data.Array as A
import Data.Maybe
import Data.Foldable (find)

main :: IO ()
main = do
  handle1 <- openFile "../inputs/02.txt" ReadMode
  file1 <- hGetContents handle1
  let i1 = fmap read . splitOn "," $ file1
  print $ toList . compute . fromList $ i1
  hClose handle1

  handle2 <- openFile "../inputs/02b.txt" ReadMode
  file2 <- hGetContents handle2
  let i2 = fmap read . splitOn "," $ file2
  print $ (\(x,y) -> 100 * x + y) <$> (findInputs i2 19690720)
  hClose handle2

data Zipper a = Zipper (Array Int a) a (Array Int a)
  deriving (Eq, Ord, Show)

fromList :: [a] -> Zipper a
fromList (x:xs) = Zipper (A.listArray (0,-1) []) x (A.listArray (0,l) xs)
  where l = length xs - 1

toList :: Zipper a -> [a]
toList (Zipper l a r) = foldl' (flip (:)) (a: elems r) (elems l)

focus :: Zipper a -> a
focus (Zipper l v r) = v

shiftR :: Zipper a -> Int -> Zipper a
shiftR z = (iterate right z !!)

shiftL :: Zipper a -> Int -> Zipper a
shiftL z = (iterate left z !!)

right :: Zipper a -> Zipper a
right z@(Zipper l v r) =
  let ls = elems l
      rs = elems r
      bl = bounds l
      br = bounds r
  in case rs of
       [] -> z
       (r':rs') -> Zipper
          (A.listArray (fmap (+1) bl) (v:ls))
          r'
          (A.listArray (fmap (flip (-) 1) br) rs')

left :: Zipper a -> Zipper a
left z@(Zipper l v r) =
  let ls = elems l
      rs = elems r
      bl = bounds l
      br = bounds r
  in case ls of
       [] -> z
       (l':ls') -> Zipper
          (A.listArray (fmap (flip (-) 1) bl) ls')
          l'
          (A.listArray (fmap (+ 1) br) (v:rs))

(%) :: Zipper a -> Int -> a
(%) (Zipper l v r) n
    | i <= 0    = l A.! abs i
    | i == 1    = v
    | otherwise = r A.! (i - 2)
  where i = n - snd (bounds l)

update :: Zipper a -> Int -> a -> Zipper a
update (Zipper l v r) n a
    | i <= 0    = Zipper (l // [(abs i, a)]) v r
    | i == 1    = Zipper l a r
    | otherwise = Zipper l v (r // [(i - 2, a)])
  where i = n - snd (bounds l)


compute :: Zipper Int -> Zipper Int
compute z = case focus z of
              99 -> z
              1  -> let a  = z % (focus $ right z)
                        a' = z % (focus $ shiftR z 2)
                        p  = focus $ shiftR z 3
                      in compute $ update (shiftR z 4) p (a + a')
              2  -> let a  = z % (focus $ right z)
                        a' = z % (focus $ shiftR z 2)
                        p  = focus $ shiftR z 3
                      in compute $ update (shiftR z 4) p (a * a')
              x  -> error $ "shouldn't have got this: " ++ (show x)

findInputs :: [Int] -> Int -> Maybe (Int, Int)
findInputs xs n = let l = (\x y -> 1:x:y:xs) <$> [0..99] <*> [0..99]
                   in case find ((== n) . (% 0) . compute . fromList) l of
                        Nothing -> Nothing
                        Just a -> Just (a !! 1, a !! 2)
