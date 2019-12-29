import Data.Intcode

main = execStdin $ last . runList [1]
