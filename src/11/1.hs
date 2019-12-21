import qualified Data.Map as M
import Data.Intcode
import Data.Int

type Position = (Int, Int)
data Robot = Robot { position :: Position, direction :: Int, panels :: M.Map Position Int64 }

main = execStdin (paintHull robot)
    where robot = Robot (0,0) 0 M.empty

paintHull rob effect =
    let
        currentColor = M.findWithDefault 0 (position rob) (panels rob)

        paint color rob@Robot{panels=hull, position=pos} =
            rob { panels = M.insert pos color hull }

        rotate f robot = robot { direction = f (direction robot) `mod` 4 }
        diff 0 = rotate (subtract 1)
        diff 1 = rotate (+1)

        stepSize = (!!) [(0, -1), (-1, 0), (0, 1), (1, 0)]
        sum (x,y) (x',y') = (x + x', y + y')

        move :: Int64 -> Robot -> Robot
        move dir robot = rot { position = sum (stepSize (direction rot)) (position rot) }
            where rot = diff dir robot

    in case effect of
         Input whatColor             -> paintHull rob $ whatColor currentColor
         Output color (Output dir p) -> flip paintHull p . move dir $ paint color rob
         End                         -> M.size $ panels rob
