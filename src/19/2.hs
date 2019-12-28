import Data.Intcode
import Data.Maybe
import Data.List
import Control.Monad
import Control.Arrow

main = execStdin run

run fx = head . mapMaybe (square fx) $ iterate (next fx) (3,4)

pulls fx (x,y) = (==1) . fst . fromOutput $ fromInput (fromInput fx $ widen x) (widen y)

next fx (x,y) =
    let y' = y + 1
        x' = fst . fromJust $ find (pulls fx) [(n, y') | n <- [x..]]
    in (x', y')

square :: Effect -> (Int, Int) -> Maybe Int
square fx (x,y) = guard (all (pulls fx) corners) >> return (x * 10000 + y - 99)
    where corners = [(x,y), (x + 99, y), (x, y - 99), (x + 99, y - 99)]
