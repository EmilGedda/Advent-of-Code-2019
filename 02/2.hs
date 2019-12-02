#!/usr/bin/env runhaskell
import Data.List.Split
import Data.Vector as V

main = print . solve 0 0 . V.map read . fromList . splitOn "," =<< getContents

solve 100 j v = solve 0 (j + 1) v
solve i j v
    | 19690720 == run 0 (v // [(1, i), (2, j)]) = 100 * i + j
    | otherwise = solve (i + 1) j v

run i v
    | v ! i == 99 = V.head v
    | otherwise = let instruction = toList $ slice i 4 v
                  in run (i + 4) $ (//) <*> (execute instruction) $ v
    where execute (opcode:a:b:dest:[]) v = [(dest, op opcode (v ! a) (v ! b))]

op = (!) $ fromList [undefined, (+), (*)]
