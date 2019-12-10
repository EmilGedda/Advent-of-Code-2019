#!/usr/bin/env runhaskell
import Control.Arrow
import Data.Ord
import Data.List hiding (insert)
import Data.Set hiding (map, foldr, filter)
import Data.Tuple

main = print . solve . parse . filter (/='\n') =<< getContents

solve xs = maximumBy (comparing fst) $ map (flip visible xs &&& id) xs

parse = map (coords . fst) . filter snd . zip [0..] . map (=='#')
    where coords = swap . (`divMod` 33)

visible origin = size . foldr (insert . angle origin) empty
    where angle o = uncurry atan2 . frac . swap . sub o
          sub (x,y) (x',y') = (x' - x, y' - y)
          frac = fromIntegral *** fromIntegral
