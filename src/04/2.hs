import Control.Monad
import Data.List

main = print . length $ filter valid [236666..699999]

valid = liftM2 (&&) sorted adjacent . digits
    where sorted xs = and $ zipWith (>=) xs (tail xs)
          adjacent = any ((==2) . length) . group

digits 0 = []
digits x = let (div, rem) = x `divMod` 10 in rem:digits div
