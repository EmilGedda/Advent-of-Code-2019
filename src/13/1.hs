import Data.Intcode
import Data.Map hiding (filter)

main = execStdin $ length . filter (==2) . elems . flip run empty
    where run (Output x (Output y (Output tile fx))) = run fx . insert (x,y) tile
          run End = id
