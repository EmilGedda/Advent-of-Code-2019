import Data.Intcode
import Data.List

main = print . maximum . flip map (permutations [5..9]) . amplify =<< fromStdin

amplify p [a',b',c',d',e'] =
    let a = runProg p (a':0:e)
        b = runProg p (b':a)
        c = runProg p (c':b)
        d = runProg p (d':c)
        e = runProg p (e':d)
    in last e
