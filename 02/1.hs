#!/usr/bin/env runhaskell
import Data.List.Split
import Data.Vector as V

main = print . run 0 . program . V.map read . fromList . splitOn "," =<< getContents

program = (// [(1, 12), (2, 2)])

run i v
    | v ! i == 99 = V.head v
    | otherwise = let instruction = toList $ slice i 4 v
                  in run (i + 4) $ (//) <*> (execute instruction) $ v
    where execute (opcode:a:b:dest:[]) v = [(dest, op opcode (v ! a) (v ! b))]

op = (!) $ fromList [undefined, (+), (*)]
