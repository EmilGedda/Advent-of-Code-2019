import Data.Int
import Data.List.Split
import Data.Maybe
import Data.Intcode hiding (pos)
import qualified Data.Map as M

type Position = (Int, Int)

main = putStr . printHull . paintHull robot . execute . parse 10000 =<< getContents
    where robot = Robot (0,0) 0 $ M.insert (0,0) 1 M.empty

data Robot = Robot { pos :: Position, dir :: Int, panels :: M.Map Position Int64 }

paintHull :: Robot -> Effect -> M.Map Position Int64
paintHull rob effect =
    let
        currentColor robot = M.findWithDefault 0 (pos robot) (panels robot)

        paint color robot@Robot{panels=hull, pos=p} =
            robot { panels = M.insert p color hull }

        rotate f robot = robot { dir = f (dir robot) `mod` 4 }
        diff 0 = rotate (subtract 1)
        diff 1 = rotate (+ 1)

        stepSize = (!!) [(0, -1), (-1, 0), (0, 1), (1, 0)]
        sum' (x,y) (x',y') = (x + x', y + y')

        move :: Int64 -> Robot -> Robot
        move d robot = rot { pos = sum' (stepSize $ dir rot) (pos rot) }
            where rot = diff d robot

    in case effect of
         Input whatColor             -> paintHull rob . whatColor $ currentColor rob
         Output color (Output dir p) -> flip paintHull p . move dir $ paint color rob
         End _                       -> panels rob


printHull :: M.Map Position Int64 -> String
printHull hull =
    let
        panels = map fst . filter ((==1) . snd) $ M.toList hull

        coord f = (,) <$> minimum <*> maximum $ map f panels
        (minx, lenx) = coord fst
        (miny, leny) = coord snd

        pixels = [(x,y) | y <- [miny..leny], x <- [lenx, lenx - 1..minx]]

        draw pixel = maybe ' ' char (M.lookup pixel hull)
        char 1 = '#'
        char 0 = ' '

    in unlines . chunksOf (lenx - minx + 1) $ map draw pixels
