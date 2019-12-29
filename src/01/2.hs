main = print . sum . map (fuel . read) . lines =<< getContents
    where fuel n | n < 9 = 0
                 | otherwise = (+) <*> fuel $ n `div` 3 - 2
