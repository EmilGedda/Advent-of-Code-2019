#!/usr/bin/env runhaskell

main = print . sum . map (fuel . read) . lines =<< getContents

fuel n
    | n < 9 = 0
    | otherwise = m + (fuel m)
    where m = n `div` 3 - 2 
