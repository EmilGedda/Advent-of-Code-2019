import Data.Map hiding (map)
import Data.List.Split

main = print . solve . map grid . init . map (splitOn ",") . splitOn "\n" =<< getContents

solve [a,b] = minimum . elems $ intersectionWith (+) a b

grid :: [String] -> Map (Int, Int) Int
grid d = grid' d 1 (0,0) empty

grid' [] _ _ set = set
grid' ((dir:dist):xs) len (x,y) set = grid' xs (len' + 1) end . unionWith min set $ fromList steps
    where steps = flip zip [len..] . zipWith coords [1..] $ replicate (read dist) (offset dir)
          coords d (a, b) = (d*a + x, d*b + y)
          (end, len') = last steps

offset 'L' = (-1, 0)
offset 'U' = (0, -1)
offset 'D' = (0, 1)
offset 'R' = (1, 0)
