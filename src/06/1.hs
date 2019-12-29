import qualified Data.Map.Strict as M
import Control.Arrow

main = print . orbitals . parse =<< getContents

orbitals m = orbits' 0 m "COM"
    where orbits' n m s = (n+) . sum . map (orbits' (n + 1) m) $ M.findWithDefault [] s m

parse = foldr parse' M.empty . lines
    where parse' = uncurry (M.insertWith (++)) . second (pure . tail) . break (')'==)
