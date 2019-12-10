#!/usr/bin/env runhaskell
import Data.Set hiding (map, foldr, filter)
import Data.Tuple

main = print . maximum . (map =<< flip visible) . parse . filter (/='\n') =<< getContents

parse = map (coords . fst) . filter snd . zip [0..] . map (=='#')
    where coords = swap . (`divMod` 33)

visible origin = size . foldr (insert . angle origin) empty
    where angle o = uncurry atan2 . frac . swap . sub o
          frac (x,y) = (fromIntegral x, fromIntegral y)
          sub (x,y) (x',y') = (x' - x, y' - y)
