#!/usr/bin/env runhaskell
import qualified Data.Map.Strict as M
import Data.List
import Data.Tuple
import Control.Arrow

main = print . diff . parse =<< getContents

diff m = let you = backtrack "YOU" m
             san = backtrack "SAN" m
         in length you + length san - 2 * length (intersect you san)
    where backtrack s m = s:(maybe [] . flip backtrack) m (M.lookup s m)

parse = foldr parse' M.empty . lines
    where parse' = uncurry M.insert . first tail . swap . break (')'==)
