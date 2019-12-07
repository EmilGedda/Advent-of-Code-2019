#!/usr/bin/env runhaskell
import Control.Monad
import Control.Monad.Loops
import Control.Exception
import Data.IORef
import Data.List
import Data.Foldable
import System.Environment
import System.Process
import System.IO

main = print =<< bruteforce  =<< getArgs

bruteforce [computer, code] = maximum <$> mapM (run computer code) (permutations [5..9])

run computer code order =
    let p = (proc computer [code]) { std_in = CreatePipe, std_out = CreatePipe }
    in do
      procs <- replicateM (length order) (createProcess p)
      zipWithM_ phase order procs

      ref <- newIORef 0

      let cycle v = writeIORef ref v >> foldM amplify v procs
          handler :: IOException -> IO Int
          handler _ = readIORef ref

      res <- iterateM_ cycle 0 `catch` handler
      mapM_ cleanupProcess procs
      return res

phase setting (Just stdin', _, _, _) = hPrint stdin' setting >> hFlush stdin'

amplify :: Int -> (Maybe Handle, Maybe Handle, c, d) -> IO Int
amplify value (Just stdin', Just stdout', _, _) = hPrint stdin' value
                                                  >> hFlush stdin'
                                                  >> (read . last . words <$> hGetLine stdout')
