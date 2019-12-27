import Data.Intcode
import Control.Monad
import Data.Maybe
import Data.Char
import qualified Data.Map.Strict as M

main = execStdin (solve . grid . lines . camera)

solve g = sum . map (fromMaybe 0 . alignParam g) $ M.keys g

camera (Output c eff) = chr (narrow c):camera eff
camera _ = []

grid = foldr (\(y,line) g -> grid' g y line) M.empty . zip [0..]
    where grid' g y = foldr (\(x,c) -> M.insert (x,y) c) g . zip [0..]

alignParam g (x,y) = do
    coords <- mapM (`M.lookup` g) [(x+1,y),(x-1,y),(x,y+1),(x,y-1),(x,y)]
    guard $ all (=='#') coords
    return (x * y)
