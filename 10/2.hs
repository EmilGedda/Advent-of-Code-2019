#!/usr/bin/env runhaskell
import Control.Arrow
import Data.Map.Strict hiding (map, foldr, filter, drop, take)
import Data.List
import Data.Tuple

main = print . solve . parse . filter (/='\n') =<< getContents

solve = uncurry ((+) . (100 *)) . (!! 199) 
        . concat . transpose . elems . visible

parse = map (coords . fst) . filter snd . zip [0..] . map (=='#')
    where coords = swap . (`divMod` 33)

visible = foldr (uncurry (insertWith (++)) . (angle &&& pure)) empty
    where angle = (-) pi . uncurry atan2 . frac . sub (23,29)
          sub (x,y) (x',y') = (x' - x, y' - y)
          frac = fromIntegral *** fromIntegral
