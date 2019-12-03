#!/usr/bin/env runhaskell
import Data.Set hiding (map)
import Data.List hiding (union)
import Data.List.Split

main = print . solve . map grid . init . map (splitOn ",") . splitOn "\n" =<< getContents

solve [a,b] = head . sort . map manhattan . toList $ intersection a b
    where manhattan (x,y) = (abs x) + (abs y)

grid :: [String] -> Set (Int, Int)
grid d = grid' d (0,0) empty

grid' [] _ set = set
grid' ((dir:dist):xs) (x,y) set = grid' xs (last steps) . union set $ fromList steps
    where steps = map coords . zip [1..] $ replicate (read dist) (offset dir)
          coords (d, (a, b)) = (d*a + x, d*b + y)

offset 'L' = (-1, 0)
offset 'U' = (0, -1)
offset 'D' = (0, 1)
offset 'R' = (1, 0)
