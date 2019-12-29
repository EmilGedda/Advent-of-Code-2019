import Data.Intcode

main = execStdin run
    where run (Input f) = run $ f 1
          run (Output x _) = x
