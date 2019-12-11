#!/usr/bin/env runhaskell
import Control.Arrow
import Data.Bool
import Data.Function
import Data.Int
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Sequence hiding (filter, reverse)
import Prelude hiding (replicate)
import qualified Data.Map as M

type Position = (Int, Int)


main = printHull . paintHull robot . execute . parse 10000 =<< getContents
    where robot = Robot (0,0) 0 M.empty

data Robot = Robot { position :: Position, direction :: Int, panels :: M.Map Position Int64 }

sum' (x,y) (x',y') = (x + x', y + y')

paintHull :: Robot -> Effect -> M.Map Position Int64
paintHull rob effect =
    let
        currentColor robot = M.findWithDefault 0 (position robot) (panels robot)

        paint color robot@Robot{panels=hull, position=pos} =
            robot { panels = M.insert pos color hull }

        rotate f robot = robot { direction = f (direction robot) `mod` 4 }
        diff 0 = rotate (subtract 1)
        diff 1 = rotate (+1)

        stepSize = (!!) [(0, -1), (-1, 0), (0, 1), (1, 0)]

        move :: Int64 -> Robot -> Robot
        move dir robot = rot { position = sum' (stepSize (direction rot)) (position rot) }
            where rot = diff dir robot

        white :: Robot -> Robot
        white robot@Robot{panels=hull} = robot { panels = M.insert (0,0) 1 hull }

    in case effect of
         Input whatColor             -> paintHull (white rob) $ whatColor (currentColor $ white rob)
         Output color (Output dir p) -> flip paintHull p . move dir $ paint color (white rob)
         End                         -> panels (white rob)


printHull :: M.Map Position Int64 -> IO  ()
printHull hull = 
    let 
        panels = map fst . filter ((==1) . snd) $ M.toList hull

        coord c f = c . map f
        minx = coord minimum fst panels
        miny = coord minimum snd panels
        lenx = coord maximum fst panels
        leny = coord maximum snd panels

        pixels = [(x,y) | x <- [minx..lenx], y <- [miny..leny]]

        draw h p = char . fromMaybe 0 $ M.lookup p h

        char 1 = '#'
        char 0 = ' '

    in mapM_ (putStrLn . reverse  . map (draw hull)) . transpose $ groupBy ((==) `on` fst) pixels

-- COMPUTER BELOW ---
data Program = Program { pos :: Int64, base :: Int64,  memory :: Seq Int64 } deriving Show

data Param = Position Int | Immediate Int64 | Relative Int

data Op = Apply (Int64 -> Int64 -> Int64) Param Param Param
        | In Param
        | Out Param
        | Halt
        | Jmp (Int64 -> Bool) Param Param
        | Cmp (Int64 -> Int64 -> Bool) Param Param Param
        | Base Param

data Effect = Input (Int64 -> Effect)
            | Output Int64 Effect
            | End



parse :: Int -> String -> Program
parse size code = Program 0 0 $ input >< Data.Sequence.replicate fill 0
        where input = fromList . map read $ splitOn "," code
              fill = max 0 $ size - Data.Sequence.length input

step f prog@Program{pos=i} = prog { pos = f i }
save i v prog@Program{memory=mem} = v `seq` prog { memory = update i v mem }

execute :: Program -> Effect
execute prog@Program{base=relbase} =
    let at i   = memory prog `index` i
        cont n = execute . step (+n)
        goto n = execute . step (const n)

        deref (Immediate i) = i
        deref (Position  i) = at i
        deref (Relative  i) = at $ i + narrow relbase
        apply f a b = f (deref a) (deref b)

        store (Immediate _) = error "Invalid to store to Immediate"
        store (Position  i) = save i
        store (Relative  i) = save (i + narrow relbase)

        base n = prog { base = relbase + n }
    in

    case decode prog of
        Apply f a b c  -> cont 4 $ store c (apply f a b) prog
        In  dest       -> Input (\x -> cont 2 $ store dest x prog)
        Out x          -> Output (deref x) $ cont 2 prog
        Jmp cond a b   -> bool (cont 3) (goto $ deref b) (cond $ deref a) prog
        Cmp cond a b c -> cont 4 $ store c (bool 0 1 $ cond (deref a) (deref b)) prog
        Base rel       -> cont 2 . base $ deref rel
        Halt           -> End

narrow ::Int64 -> Int
narrow = fromIntegral

decode :: Program -> Op
decode prog =
    let (modes, opcode) = arg 0 `divMod` 100
        arg i   = memory prog `index` (narrow (pos prog) + i)
        mode i  = modes `div` (10 ^ i) `mod` 10
        param i =
            case mode (i - 1) of
                0 -> Position  $ fromIntegral (arg i)
                1 -> Immediate $ fromIntegral (arg i)
                2 -> Relative  $ fromIntegral (arg i)
                _ -> error $ "Invalid mode: " ++ show i
    in

    case opcode of
        1  -> Apply (+) (param 1) (param 2) (param 3)
        2  -> Apply (*) (param 1) (param 2) (param 3)
        3  -> In        (param 1)
        4  -> Out       (param 1)
        5  -> Jmp (/=0) (param 1) (param 2)
        6  -> Jmp (==0) (param 1) (param 2)
        7  -> Cmp (<)   (param 1) (param 2) (param 3)
        8  -> Cmp (==)  (param 1) (param 2) (param 3)
        9  -> Base      (param 1)
        99 -> Halt
        _  -> error $ "Invalid opcode: " ++ show opcode