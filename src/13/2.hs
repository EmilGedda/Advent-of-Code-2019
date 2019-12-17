import Data.Intcode
import Data.Map hiding (filter)

main = execStdinWith (save 0 2) $ run empty

run g End = g ! (-1,0)
run g (Output x (Output y (Output tile fx))) = run (insert (x,y) tile g) fx
run g (Input joystick) = run g $ joystick . signum $ xOf 4 - xOf 3
    where xOf t = fst . fst .  head . filter ((==) t . snd) $ toList g
