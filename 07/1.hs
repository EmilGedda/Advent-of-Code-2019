#!/usr/bin/env runhaskell
import Control.DeepSeq
import Data.List
import System.Environment
import System.Process
import System.IO

main = print =<< bruteforce  =<< getArgs

bruteforce [computer, code] = maximum <$> mapM (thrust computer code) (permutations [0..4])

thrust :: String -> String -> [Int] -> IO Int
thrust computer code = foldr ((=<<) . run computer code) (return 0)

run :: String -> String -> Int -> Int -> IO Int
run computer code amplifier input =
    let p = (proc computer [code]) { std_in = CreatePipe, std_out = CreatePipe }
    in withCreateProcess p $ \mbstdin mbstdout _ _ -> do

        let Just stdin'  = mbstdin
            Just stdout' = mbstdout

        hSetBuffering stdin'  NoBuffering

        hPrint stdin' amplifier
        hPrint stdin' input

        s <- hGetContents stdout'
        return . read . last $!! words s
