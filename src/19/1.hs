import Data.Intcode

main = execStdin
        $ sum
        . concat
        . zipWith ($) [ runList [x,y] | x <- [0..49], y <- [0..49] ]
        . repeat
