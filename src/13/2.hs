import Data.Intcode
import Data.Maybe
import qualified Data.Map as M

main = execStdinWith (save 0 2) $ run M.empty

run grid effect =
    let find t = fst . fst .  head . filter ((==) t . snd) $ M.toList grid
    in case effect of
         Input joystick -> run grid . joystick . signum $ find 4 - find 3
         End            -> fromMaybe 0 $ M.lookup (-1,0) grid
         Output x (Output y (Output tile fx)) -> flip run fx $ M.insert (x,y) tile grid
