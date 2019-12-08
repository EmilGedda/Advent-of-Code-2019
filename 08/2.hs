#!/usr/bin/env runhaskell
import Data.Bool
import Data.List.Split
import Data.Char

width = 25
height = 6
area = width * height

main = print . foldl1 (zipWith flatten) . chunksOf area . map digitToInt . init =<< getContents
    where print = mapM putStrLn . chunksOf width . map char
          flatten a b = bool a b (a == 2)
          char 0 = ' '
          char 1 = '#'
