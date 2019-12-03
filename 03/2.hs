#!/usr/bin/env runhaskell
import Data.HashMap.Strict hiding (map)
import Data.List
import Data.List.Split

main = print . solve . map grid . init . map (splitOn ",") . splitOn "\n" =<< getContents

solve [a,b] = head . sort . map snd . toList $ intersectionWith (+) a b

grid d = grid' d 1 (0,0) empty

grid' [] _ _ set = set
grid' ((dir:dist):xs) len (x,y) set = let (end, len') = last steps
          in grid' xs (len' + 1) end . unionWith min set $ fromList steps
    where steps = flip zip [len..] . map coords . zip [1..] $ replicate (read dist) (offset dir)
          coords (d, (a, b)) = (d*a + x, d*b + y)

offset 'L' = (-1, 0)
offset 'U' = (0, -1)
offset 'D' = (0, 1)
offset 'R' = (1, 0)
