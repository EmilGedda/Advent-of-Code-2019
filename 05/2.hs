#!/usr/bin/env runhaskell
import Control.Arrow
import Data.Bool
import Data.List.Split hiding (split)
import Data.Vector hiding (drop, head, map, reverse, zip, replicate, (++), length, last, zipWith)
import Data.Tuple
import System.Environment

main :: IO ()
main = run . parseProgram =<< readFile . head  =<< getArgs

data Mode = Position | Immediate
data Operation = Apply  (Int -> Int -> Int) | In | Out | Halt
               | Jump Bool | Compare (Int -> Int -> Bool) | Equals
data Instruction = Instruction Operation Parameters
data Parameter = Parameter { mode :: Mode, value :: Int }
data Program = Program { code :: Vector Int, index :: Int }

type Parameters = [Parameter]

opLength :: Operation -> Int
opLength (Apply _)   = 3
opLength In          = 1
opLength Out         = 1
opLength Halt        = 0
opLength (Jump _)    = 2
opLength (Compare _) = 3

modeType :: Int -> Mode
modeType 0 = Position
modeType 1 = Immediate

opcode :: Int -> Operation
opcode 99 = Halt
opcode x  = fromList [undefined, Apply (+), Apply (*), In, Out
                     , Jump True, Jump False, Compare (<), Compare (==) ] ! x

parseParameter :: Mode -> Program -> Parameter
parseParameter m p = Parameter m (current p)

parseInstruction :: Program -> Instruction
parseInstruction p = uncurry Instruction . second params . split $ current p
    where params :: [Mode] -> Parameters
          params modes = zipWith parseParameter modes . drop 1 $ iterate (step 1) p

split :: Int -> (Operation, [Mode])
split x = let (modes, op) = second opcode $ x `divMod` 100
          in (op, map modeType . rightpad (opLength op) $ digits modes)

current :: Program -> Int
current (Program code index) = code ! index

load :: Program -> Parameter -> Int
load _ (Parameter Immediate value) = value
load (Program code _) (Parameter Position value) = code ! value

step :: Int -> Program -> Program
step n p@(Program _ i) = p { index = i + n }

next :: Instruction -> Program -> Program
next (Instruction op _) p = flip step p . (+1) $ opLength op

cont = return . Just -- continue execution

execute :: Program -> Instruction -> IO (Maybe Program)
execute p (Instruction (Apply f) params)   = cont $ apply f p params
execute p (Instruction In [dest])          = Just . set p (value dest) <$> input
execute p (Instruction Out [dest])         = print (load p dest) >> return ( Just p)
execute p (Instruction Halt [])            = return Nothing
execute p (Instruction (Jump cond) params) = cont $ jmp cond p params
execute p (Instruction (Compare c) params) = cont $ cmp c p params

jmp :: Bool -> Program -> Parameters -> Program
jmp cond p params = bool p (jump p dest) . (cond==) $ boolean x
    where jump prog i = prog { index = i }
          [x, dest] = map (load p) params
          boolean 0 = False
          boolean _ = True

cmp :: (Int -> Int -> Bool) -> Program -> Parameters -> Program
cmp comparison p params = store . bool 0 1 $ comparison a b
    where [a,b,_] = map (load p) params
          store = set p (value $ last params)

apply :: (Int -> Int -> Int) -> Program -> Parameters -> Program
apply f prog@(Program code _) [a,b,dest] = set prog address result
    where address = value dest -- position mode is actually immediate mode
          result = f (load prog a) (load prog b)

input :: IO Int
input = putStr "Enter input: " >> read <$> getLine

run :: Program -> IO ()
run prog = maybe (return ()) run =<< run' prog
   where run' p' = let i = parseInstruction p' in fmap (jump i p') <$> execute p' i
         jump i (Program _ i1) after@(Program _ i2) = bool after (next i after) (i1 == i2)

parseProgram :: String -> Program
parseProgram = flip Program 0 . fromList . map read . splitOn ","

digits :: Int -> [Int]
digits 0 = []
digits x = let (div, rem) = x `divMod` 10 in rem:digits div

rightpad n xs = xs ++ replicate (n - length xs) 0

set :: Program -> Int -> Int -> Program
set p@(Program code _) i n = p { code = adjust code i n }

adjust :: Vector a -> Int -> a -> Vector a
adjust v i n = v // [(i, n)]
