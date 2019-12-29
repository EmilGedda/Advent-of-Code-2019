import Data.Intcode
import Data.List

main = print . maximum . flip map (permutations [5..9]) . amplify =<< fromStdin

amplify prog [a',b',c',d',e'] =
    let a = toFn prog (a':0:e)
        b = toFn prog (b':a)
        c = toFn prog (c':b)
        d = toFn prog (d':c)
        e = toFn prog (e':d)
    in last e
