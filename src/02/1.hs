import Data.Intcode

main = execStdinWith (save 1 12 . save 2 2) (flip load 0 . fromEnd)
