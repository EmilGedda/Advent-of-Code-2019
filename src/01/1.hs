#!/usr/bin/env runhaskell

main = print . sum . map (subtract 2 . flip div 3 . read) . lines =<< getContents
