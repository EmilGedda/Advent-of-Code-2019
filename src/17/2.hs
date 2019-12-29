import Control.Arrow
import Control.Monad
import Data.Char
import Data.Function
import Data.Intcode
import Data.List
import Data.Maybe
import Data.Tuple
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Robot = Robot { dir :: Int, pos :: (Int, Int), vis :: S.Set (Int, Int), walk :: [((Int,Int),Int)] } deriving Show

add (x,y) (x',y') = (x + x', y + y')
sub (x,y) (x',y') = (x - x', y - y')
iterateMaybe f = unfoldr (fmap (join (,)) . f)

direction = flip elemIndex "^>v<"
step = [(0, -1), (1, 0), (0, 1), (-1, 0)]

rotated 0 1 = 'R'
rotated 0 3 = 'L'
rotated 1 2 = 'R'
rotated 1 0 = 'L'
rotated 2 1 = 'L'
rotated 2 3 = 'R'
rotated 3 2 = 'L'
rotated 3 0 = 'R'

toStr :: (Int, Char) -> String
toStr (0, dir) = [dir]
toStr (len, dir) = dir:"," ++ show len

toCode = intercalate "," . map toStr

prioritize direction p =
    let next = step !! direction
        forward = add p next
        left    = sub p (swap next)
        right   = add p (swap next)
    in [forward, left, right]

move g robot@Robot{dir=direction, pos=position, vis = visited, walk = ws} =
    let
        reachable = prioritize direction position
        tiles = length . filter (=='#') $ M.elems g
        toChar = fromMaybe '.' . flip M.lookup g
        next = dropWhile ((/='#') . toChar) reachable
    in do
    guard $ S.size visited < tiles
    newpos <- listToMaybe next
    newdir <- (`mod` 4) <$> elemIndex (sub newpos position) step
    return $ robot { pos = newpos, dir = newdir, walk = (newpos,newdir):ws
                   , vis = S.insert newpos visited }

main =
    let inputs = map (widen . ord)
               . compress [] Nothing Nothing Nothing
               . solve
               . grid
               . lines
               . map (chr . narrow)
               . runList []
               . execute

    in print . last . (runProg . save 0 2 <*> inputs) =<< fromStdin

compress acc a b c [] =
    unlines [ intercalate "," (reverse acc)
            , maybe "" toCode a
            , maybe "" toCode b
            , maybe "" toCode c, "n"]

compress acc a b c xs =
  do (ys,zs) <- tail $ liftM2 zip inits tails xs
     guard $ length (toCode ys) <= 20
     let branch
            | Just ys == a = compress ("A":acc) a b c zs
            | Just ys == b = compress ("B":acc) a b c zs
            | Just ys == c = compress ("C":acc) a b c zs
            | isNothing a  = compress ("A":acc) (Just ys) b c zs
            | isNothing b  = compress ("B":acc) a (Just ys) c zs
            | isNothing c  = compress ("C":acc) a b (Just ys) zs
            | otherwise    = []

     branch

solve g =
    let
        command xs@((_,d):_) = (length xs, d)
        paths = reverse .  map command . groupBy ((==) `on` snd)
              . walk . last . iterateMaybe (move g) $ mkRobot g
        distance (_, from) (len, to) = (len, rotated from to)
    in zipWith distance paths (drop 1 paths)

mkRobot g =
    let isRobot = isJust . direction
        start@(pos, d) = second (fromJust . direction) . head
                       . filter (isRobot . snd) $ M.toList g
    in Robot d pos S.empty [start]

grid = foldr column M.empty . zip [0..]
    where column (y,line) g = foldr (row y) g $ zip [0..] line
          row y (x,c) = M.insert (x,y) c

