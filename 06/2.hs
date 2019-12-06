#!/usr/bin/env runhaskell
import qualified Data.Map.Strict as M
import Data.Tuple
import Control.Arrow

main = print . diff . parse =<< getContents

diff = (diff' . backtrack "YOU") <*> backtrack "SAN"
    where diff' (x:xs) (y:ys) | x == y = diff' xs ys
                              | otherwise = length xs + length ys

backtrack s = reverse . backtrack' s
    where backtrack' s m = s:(maybe [] . flip backtrack') m (M.lookup s m)

parse = foldr parse' M.empty . lines
    where parse' = uncurry M.insert . first tail . swap . break (')'==)
