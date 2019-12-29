import Data.Intcode
import Data.List

main = print . maximum . flip map (permutations [0..4]) . amplify =<< fromStdin
    where amplify prog = foldr (\phase signal -> head $ toFn prog [phase, signal]) 0
