import Data.Intcode
import Data.List

main = print . maximum . flip map (permutations [0..4]) . amplify =<< fromStdin
    where amplify p = foldr (\phase signal -> head $ runProg p [phase, signal]) 0
