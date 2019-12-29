import Data.Intcode
import Data.Maybe
import Data.List
import Control.Monad

main = execStdin $ \fx -> head . mapMaybe (square fx) $ iterate (next fx) (3,4)

pulls  fx coord = (==1) . head $ runList coord fx
next   fx (x,y) = (head . fromJust $ find (pulls fx) [[n, y + 1] | n <- [x..]], y + 1)
square fx (x,y) = guard (pulls fx [x,y] && pulls fx [x+99,y-99]) >> return (x * 10000 + y - 99)
