import Data.Intcode
import Data.Maybe
import qualified Data.Map as M

main = execStdinWith (save 0 2) $ flip run M.empty

run End g = fromMaybe 0 $ M.lookup (-1,0) g
run (Output x (Output y (Output tile fx))) g = run fx $ M.insert (x,y) tile g
run (Input joystick) g = run (joystick . signum $ xOf 4 - xOf 3) g
    where xOf t = fst . fst .  head . filter ((==) t . snd) $ M.toList g
