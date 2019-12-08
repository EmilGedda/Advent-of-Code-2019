import Data.Sequence
import Data.Bool

data Program = Program { memory :: Seq Int, pos :: Int }

data Param = Position Int | Immediate Int
data Op = Apply (Int -> Int -> Int) Param Param Param
        | In Param
        | Out Param
        | Halt
        | Jmp (Int -> Bool) Param Param
        | Cmp (Int -> Int -> Bool) Param Param Param

data Effect = Input (Int -> Effect)
            | Output Int Effect
            | End

step f prog@Program{pos=i} = prog { pos = f i }
save i v prog@Program{memory=mem} = prog { memory = update i v mem }

execute :: Program -> Effect
execute prog =
    let at i   = memory prog `index` i
        cont n = execute . step (+n)
        goto n = execute . step (const n)

        deref (Immediate i) = i
        deref (Position i)  = at i
        apply f a b = f (deref a) (deref b)

        store (Immediate _) = error "Invalid to store to Immediate"
        store (Position i)  = save i
    in

    case decode prog of
        Apply f a b c  -> cont 4 $ store c (apply f a b) prog
        In  dest       -> Input (\input -> cont 2 $ store dest input prog)
        Out x          -> Output (deref x) $ cont 2 prog
        Jmp cond a b   -> bool (cont 3) (goto $ deref b) (cond $ deref a) prog
        Cmp cond a b c -> cont 4 $ store c (bool 0 1 $ cond (deref a) (deref b)) prog
        Halt           -> End

decode :: Program -> Op
decode prog =
    let (modes, opcode) = arg 0 `divMod` 100
        arg i   = memory prog `index` i + pos prog
        mode i  = modes `div` 10 ^ i `mod` 10
        param i =
            case mode i of
                0 -> Position  (arg i)
                1 -> Immediate (arg i)
                _ -> error $ "Invalid mode: " ++ show i
    in

    case opcode of
        1  -> Apply (+) (param 1) (param 2) (param 3)
        2  -> Apply (*) (param 1) (param 2) (param 3)
        3  -> In  (param 1)
        4  -> Out (param 1)
        5  -> Jmp (==1) (param 1) (param 2)
        6  -> Jmp (==0) (param 1) (param 2)
        7  -> Cmp (<)   (param 1) (param 2) (param 3)
        8  -> Cmp (<=)  (param 1) (param 2) (param 3)
        99 -> Halt
        _  -> error $ "Invalid opcode: " ++ show opcode
