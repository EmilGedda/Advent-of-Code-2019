import Data.Intcode
import Control.Monad
import Data.Maybe
import Data.Char
import qualified Data.Map.Strict as M

main = execStdin $ solve . grid . lines . map (chr . narrow) . runList []
    where solve g = sum . map (fromMaybe 0 . alignParam g) $ M.keys g

grid = foldr column M.empty . zip [0..]
    where column (y,line) g = foldr (row y) g $ zip [0..] line
          row y (x,c) = M.insert (x,y) c

alignParam g (x,y) = do
    coords <- mapM (`M.lookup` g) [(x+1,y),(x-1,y),(x,y+1),(x,y-1),(x,y)]
    guard $ all (=='#') coords
    return (x * y)
