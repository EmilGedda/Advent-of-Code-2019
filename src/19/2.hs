import Data.Intcode
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad

main = execStdin $ \fx -> head . mapMaybe (square fx) $ iterate (next fx) (3,4)

pulls :: Effect -> [Int64] -> Bool
pulls fx coord = (==1) . head $ runList coord fx

next :: Effect -> (Int64, Int64) -> (Int64, Int64)
next fx (x,y) =
    let y' = y + 1
        x' = head . fromJust $ find (pulls fx) [[n, y'] | n <- [x..]]
    in (x', y')

square :: Alternative f => Effect -> (Int64, Int64) -> f Int64
square fx (x,y) = guard (pulls fx [x, y])
               *> guard (pulls fx [x + 99, y - 99])
               *> pure (x * 10000 + y - 99)
