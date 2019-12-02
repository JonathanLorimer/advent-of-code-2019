module Adv01 where

import System.IO
import Data.List (lines)
import Data.Function (fix)

main :: IO ()
main = do
  fp <- getContents
  handle <- openFile "../inputs/01.txt" ReadMode
  file <- hGetContents handle
  print . sum . fmap (calculate . read) $ lines file
  print . sum . fmap (calculateR . read) $ lines file
  hClose handle



calculate :: Int -> Int
calculate = flip (-) 2 . flip div 3

calculateR :: Int -> Int
calculateR = fix
  (\f n -> let calc = calculate n
            in
              if calc <= 0
                then 0
                else calc + f calc)

