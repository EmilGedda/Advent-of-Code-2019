import Data.Char
import Data.List

main = putStrLn . map intToDigit . solve . map digitToInt
     . concat . replicate 10000 . init =<< getContents

solve s = take 8 . reverse . (!! 100) . iterate fft . reverse $ drop offset s
    where offset = read . map intToDigit $ take 7 s
          fft = map (`mod` 10) . scanl' (+) 0
