#!/usr/bin/env runhaskell
import qualified Data.Map.Strict as M
import Data.List
import Data.Tuple
import Control.Arrow

main = print . diff . parse =<< getContents

diff m = let you = backtrack m "YOU"
             san = backtrack m "SAN"
         in length you + length san - 2 * length (intersect you san) - 2
    where backtrack m s = s:maybe [] (backtrack m) (M.lookup s m)

parse = foldr parse' M.empty . lines
    where parse' = uncurry M.insert . first tail . swap . break (')'==)
