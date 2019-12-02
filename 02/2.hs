#!/usr/bin/env runhaskell

import Prelude hiding (head, map, dropWhile)
import Control.Arrow
import Data.List.Split
import Data.Vector

main = print . solve . map read . fromList . splitOn "," =<< getContents

solve v = uncurry (+) . first (* 100) . fst
          . head . dropWhile ((/=19690720) . snd)
          $ map (id &&& (run 0 . program v)) combinations

combinations = fromList [(x,y) | x <- [1..99], y <- [1..99]]

program v (a, b) = v // [(1, a), (2, b)]

run i v
    | v ! i == 99 = head v
    | otherwise = let instruction = toList $ slice i 4 v
                  in run (i + 4) $ v // (execute v instruction)
    where execute v (opcode:a:b:dest:[]) = [(dest, op opcode (v ! a) (v ! b))]

op n x y
    | n == 1 = x + y
    | n == 2 = x * y
