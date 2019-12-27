import Data.Intcode

main = execStdin $ sum . flip map [ (x,y) | x <- [0..49], y <- [0..49] ] . run

run fx (x,y) = out $ input (input fx x) y
    where input (Input f) = f
          out (Output x _) = x
