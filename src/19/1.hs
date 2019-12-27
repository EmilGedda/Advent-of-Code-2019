import Data.Intcode

main = execStdin $ sum . flip map [ (x,y) | x <- [0..49], y <- [0..49] ] . run
    where run fx (x,y) = fst . fromOutput $ fromInput (fromInput fx x) y
