import Data.Intcode
import Data.List

main = print . maximum . flip map (permutations [5..9]) . amplify . runProg =<< fromStdin

amplify prog [a',b',c',d',e'] =
    let a = prog (a':0:e)
        b = prog (b':a)
        c = prog (c':b)
        d = prog (d':c)
        e = prog (e':d)
    in last e
