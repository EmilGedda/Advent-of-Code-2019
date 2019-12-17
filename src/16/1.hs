#!/usr/bin/env runhaskell
import Data.Char

main = putStrLn . map intToDigit . take 8 . (!! 100) . iterate phase . map digitToInt . init =<< getContents
    where phase s = map ((`mod` 10) . abs . sum . zipWith (*) s . pattern) [0 .. length s - 1]
          pattern n = drop 1 $ concatMap (replicate (n + 1)) $ cycle [0,1,0,-1]
