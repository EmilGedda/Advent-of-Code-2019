import Data.Intcode
import Data.Maybe
import Control.Monad
import Data.Set

step = [(-1, 0), (1, 0), (0, -1), (0, 1)]
add (x, y) (x', y') = (x + x', y + y')

main = execStdin $ minimum . catMaybes . run (0,0) empty 0

run pos g s (Input cmd) = do
    direction  <- [0..3]

    let next = add pos $ step !! direction
        steps = s + 1

    guard . not $ member next g
    case cmd . widen $ direction + 1 of
        Output 0 _  -> return Nothing
        Output 1 fx -> run next (insert next g) steps fx
        Output 2 _  -> return $ Just steps
