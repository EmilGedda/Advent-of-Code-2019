#!/usr/bin/env runhaskell
import Data.Bool
import Data.List.Split
import Data.Char

width = 25
height = 6
area = width * height

main = printMsg . foldl1 color . chunksOf area . map digitToInt . init =<< getContents
    where color = zipWith flatten
          flatten a b = bool a b (a == 2)

printMsg [] = return ()
printMsg m = let (row, rest) = splitAt width m
             in putStrLn (map char row) >> printMsg rest
    where
        char 0 = ' '
        char 1 = '#'
