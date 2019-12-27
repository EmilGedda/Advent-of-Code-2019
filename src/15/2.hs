import Data.Intcode
import Control.Monad
import qualified Data.Map as M

step = [(-1, 0), (1, 0), (0, -1), (0, 1)]

add (x, y) (x', y') = (x + x', y + y')

main = execStdin $ maximum . oxygen . run (0,0) M.empty

run coord g (Input cmd) = foldr1 M.union $ do
    direction  <- [0..3]

    let next = add coord $ step !! direction
        grid c = M.insert next c g

    guard . not $ M.member next g
    return $ case cmd . widen $ direction + 1 of
        Output 0 _  -> grid '#'
        Output 1 fx -> run next (grid '.') fx
        Output 2 fx -> run next (grid 'X') fx

oxygen g = dfs 0 g . fst . head . filter ((=='X') . snd) $ M.toList g

dfs steps g pos = do
    next <- add pos <$> step
    case M.lookup next g of
        Nothing  -> return steps
        Just '#' -> return steps
        Just  _  -> dfs (steps + 1) (M.delete next g) next
