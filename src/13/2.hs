import Data.Intcode
import Data.Map hiding (filter)

main = execStdinWith (save 0 2) (run empty)

run g (End _) = last $ keys g
run g (Input joystick) = run g . joystick . signum $ (g ! 4) - (g ! 3)
run g (Output x (Output _ (Output tile fx))) = run (insert tile x g) fx
