import Data.Bool
import Data.List.Split
import Data.Char

main = print . foldl1 (zipWith flatten) . chunksOf (25 * 6) . map digitToInt . init =<< getContents
    where print = mapM putStrLn . chunksOf 25 . map char
          flatten a b = bool a b (a == 2)
          char 0 = ' '
          char 1 = '#'
