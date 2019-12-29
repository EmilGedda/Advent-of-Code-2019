import Data.Intcode
import Data.List
import Data.Maybe

main = do
    prog <- fromStdin
    let run x y = flip load 0 . fromEnd . execute . save 1 x $ save 2 y prog
        pairs = [ (x,y) | x <- [0..99], y <- [0..99] ]
        join (a,b) = 100 * a + b
    print . join . fromJust $ find ((==19690720) . uncurry run) pairs
