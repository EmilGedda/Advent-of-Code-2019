#!/usr/bin/env runhaskell
import Control.Arrow
import Data.List.Split hiding (split)
import Data.Vector hiding (drop, head, map, reverse, zip, replicate, (++), length, last)
import Data.Tuple
import System.Environment

main :: IO ()
main = run . parseProgram =<< readFile . head  =<< getArgs

data Mode = Position | Immediate                            deriving (Show)
data Operation = Add | Multiply | In | Out | Halt           deriving (Show)
data Instruction = Instruction Operation Parameters         deriving (Show)
data Parameter = Parameter { mode :: Mode, value :: Int }   deriving (Show)
data Program = Program { code :: Vector Int, index :: Int } deriving (Show)

type Parameters = [Parameter]

opLength :: Operation -> Int
opLength Add      = 3
opLength Multiply = 3
opLength In       = 1
opLength Out      = 1
opLength Halt     = 0

modeType :: Int -> Mode
modeType 0 = Position
modeType 1 = Immediate

opcode :: Int -> Operation
opcode 99 = Halt
opcode x  = fromList [undefined, Add, Multiply, In, Out] ! x

parseParameter :: Mode -> Program -> Parameter
parseParameter m p = Parameter m (current p)

parseInstruction :: Program -> Instruction
parseInstruction p = uncurry Instruction . second params . split $ current p
    where params :: [Mode] -> Parameters
          params modes = map (uncurry parseParameter) . zip modes . drop 1 $ iterate (step 1) p

split :: Int -> (Operation, [Mode])
split x = let (modes, op) = second opcode $ x `divMod` 100
          in (op, map modeType . rightpad (opLength op) $ digits modes)

current :: Program -> Int
current (Program code index) = code ! index

load :: Program -> Parameter -> Int
load _ (Parameter Immediate value) = value
load (Program code _) (Parameter Position value) = code ! value

step :: Int -> Program -> Program
step n (Program code index) = Program code (index + n)

next :: Instruction -> Program -> Program
next (Instruction op _) p = flip step p . (+1) $ opLength op

execute :: Program -> Instruction -> IO (Either Program Program)
execute p (Instruction Add params)      = return . Right $ apply (+) p params
execute p (Instruction Multiply params) = return . Right $ apply (*) p params
execute p (Instruction In [dest])       = Right . set p (value dest) <$> input
execute p (Instruction Out [dest])      = print (load p dest) >> (return $ Right p)
execute p (Instruction Halt [])         = return $ Left p

apply :: (Int -> Int -> Int) -> Program -> Parameters -> Program
apply f prog@(Program code _) [a,b,dest] = set prog address result
    where address = value dest -- position mode is actually immediate mode
          result = f (load prog a) (load prog b)

input :: IO Int
input = putStr "Enter input: " >> read <$> getLine

run :: Program -> IO ()
run prog = do
        result <- run' prog
        case result of
            Right p -> run p
            Left p -> return ()
   where run' p' = let i = parseInstruction p' in (fmap.fmap) (next i) $ execute p' i

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
