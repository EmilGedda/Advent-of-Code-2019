#!/usr/bin/env runhaskell
import Data.Set hiding (map, foldr, filter)

main = print . maximum . (map =<< flip visible) . parse . filter (/='\n') =<< getContents
    where parse = map ((`divMod` 33) . fst) . filter snd . zip [0..] . map (=='#')

visible origin = size . foldr (insert . angle origin) empty
    where angle o = uncurry atan2 . frac . sub o
          frac (x,y) = (fromIntegral x, fromIntegral y)
          sub (x,y) (x',y') = (x' - x, y' - y)
