#!/usr/bin/env runhaskell
import Control.Monad
import Data.List
main = print . length $ filter validPassword [236666..699999]

validPassword = liftM2 (&&) sortedDescending adjacent . digits
    where sortedDescending xs = and $ zipWith (>=) xs (tail xs)
          adjacent = or . map ((==2) . length) . group

digits 0 = []
digits x = let (div, rem) = x `divMod` 10 in rem:digits div
