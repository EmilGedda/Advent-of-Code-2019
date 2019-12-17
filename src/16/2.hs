#!/usr/bin/env runhaskell
{-# LANGUAGE BangPatterns #-}
import Data.Char

main = putStrLn . map intToDigit .  solve . map digitToInt . concat . replicate 10000 . init =<< getContents

solve s = 
    let (!init, !msg) = splitAt 7 s 
        offset = read $ map intToDigit init
    in take 8 . drop offset . (!! 100) $ iterate phase msg

phase !s = map ((`mod` 10) . abs . sum . zipWith (*) s . pattern) [0 .. length s - 1]
pattern n = drop 1 $ concatMap (replicate (n + 1)) $ cycle [0,1,0,-1]
