import Data.List
import Data.Ord
import Data.List.Split
import Data.Char

main = print . solve . chunksOf (25 * 6) . map digitToInt . init =<< getContents

solve img = count 1 layer * count 2 layer
    where layer = minimumBy (comparing (count 0)) img
          count n = length . filter (n==)
