module Aoc.Day24(solve) where
import Control.Monad.Trans.State.Strict (State, get, put, evalState)

type Indata = [Instruction]

data Register = RegX | RegY | RegZ | RegW deriving Show

data Dest = Literal Int | Reg Register deriving Show

data Instruction
    = IInput Register
    | IAdd Register Dest
    | IMul Register Dest
    | IDiv Register Dest
    | IMod Register Dest
    | IEql Register Dest
    deriving Show

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    putStrLn $ part1 parsed
--    putStrLn $ part2 parsed

readReg :: String -> Register
readReg "x" = RegX
readReg "y" = RegY
readReg "z" = RegZ
readReg "w" = RegW
readReg _ = error "unknown register"

readDest :: String -> Dest
readDest "x" = Reg RegX
readDest "y" = Reg RegY
readDest "z" = Reg RegZ
readDest "w" = Reg RegW
readDest s = Literal $ read s

parseInst :: String -> Instruction
parseInst l = case words l of
    "inp":a:_ -> IInput (readReg a)
    "add":a:b:_ -> IAdd (readReg a) (readDest b)
    "mul":a:b:_ -> IMul (readReg a) (readDest b)
    "div":a:b:_ -> IDiv (readReg a) (readDest b)
    "mod":a:b:_ -> IMod (readReg a) (readDest b)
    "eql":a:b:_ -> IEql (readReg a) (readDest b)
    _ -> error "unknown instruction"

parseData :: String -> Indata
parseData input = parseInst <$> lines input

genVar :: State Char Char
genVar = do
    c <- get
    put $ succ c
    pure c

part1 :: Indata -> String
part1 insns = show . (\(_,_,z,_)->z) . flip evalState 'a' $ runProgramSymbolic genVar insns (Variable 'x', Variable 'y', Variable 'z', Variable 'w')

data Number
    = Variable Char
    | Number Int
    | Add Number Number
    | Div Number Number
    | Mul Number Number
    | Mod Number Number
    | Eql Number Number

simplifyNumber :: Number -> Number
simplifyNumber (Add (Number 0) x) = x
simplifyNumber (Add x (Number 0)) = x
simplifyNumber (Add (Number a) (Number b)) = Number $ a + b
simplifyNumber (Div (Number a) (Number b)) = Number $ a `div` b
simplifyNumber (Div x (Number 1)) = x
simplifyNumber (Mul (Number a) (Number b)) = Number $ a * b
simplifyNumber (Mul (Number 0) _) = Number 0
simplifyNumber (Mul _ (Number 0)) = Number 0
simplifyNumber (Mul (Number 1) x) = x
simplifyNumber (Mul x (Number 1)) = x
simplifyNumber (Mod (Number a) (Number b)) = Number $ a `rem` b
simplifyNumber (Eql (Number a) (Number b)) = Number $ if a == b then 1 else 0
simplifyNumber n = n


instance Show Number where
    show (Variable c) = [c]
    show (Number n) = show n
    show (Add a b) = "(" <> show a <> " + " <> show b <> ")"
    show (Div a b) = "(" <> show a <> " / " <> show b <> ")"
    show (Mul a b) = "(" <> show a <> " * " <> show b <> ")"
    show (Mod a b) = "(" <> show a <> " % " <> show b <> ")"
    show (Eql a b) = "(" <> show a <> " == " <> show b <> " ? 1 : 0)"

numberSize :: Number -> Int
numberSize (Variable _) = 1
numberSize (Number _) = 1
numberSize (Add a b) = numberSize a + numberSize b
numberSize (Div a b) = numberSize a + numberSize b
numberSize (Mul a b) = numberSize a + numberSize b
numberSize (Mod a b) = numberSize a + numberSize b
numberSize (Eql a b) = numberSize a + numberSize b

data Constraint
    = Constraint ConstraintKind Number Int

constraintSize :: Constraint -> Int
constraintSize (Constraint _ n _) = numberSize n

data ConstraintKind = Eq | Neq | Lt

instance Show Constraint where
    show (Constraint kind a b) = show a <> " " <> show kind <> " " <> show b

instance Show ConstraintKind where
    show Eq = "=="
    show Neq = "/="
    show Lt = "<"

updateRegister :: (Number, Number, Number, Number) -> Register -> Number -> (Number, Number, Number, Number)
updateRegister (_, regY, regZ, regW) RegX val = (simplifyNumber val, regY, regZ, regW)
updateRegister (regX, _, regZ, regW) RegY val = (regX, simplifyNumber val,  regZ, regW)
updateRegister (regX, regY, _, regW) RegZ val = (regX, regY, simplifyNumber val,  regW)
updateRegister (regX, regY, regZ, _) RegW val = (regX, regY, regZ, simplifyNumber val)

fetchRegister :: (Number, Number, Number, Number) -> Dest -> Number
fetchRegister (regX, _, _, _) (Reg RegX) = regX
fetchRegister (_, regY, _, _) (Reg RegY) = regY
fetchRegister (_, _, regZ, _) (Reg RegZ) = regZ
fetchRegister (_, _, _, regW) (Reg RegW) = regW
fetchRegister _ (Literal n) = Number n

runProgramSymbolic :: State a Char -> [Instruction] -> (Number, Number, Number, Number) -> State a (Number, Number, Number, Number)
runProgramSymbolic _ [] regs = pure regs
runProgramSymbolic genVariable ((IInput dest):is) regs = do
    newVar <- genVariable
    runProgramSymbolic genVariable is (updateRegister regs dest (Variable newVar))
runProgramSymbolic genVariable ((IAdd dest source):is) regs = do
    let newRegs = updateRegister regs dest $ Add (fetchRegister regs (Reg dest)) (fetchRegister regs source)
    runProgramSymbolic genVariable is newRegs
runProgramSymbolic genVariable ((IMul dest source):is) regs = do
    let newRegs = updateRegister regs dest $ Mul (fetchRegister regs (Reg dest)) (fetchRegister regs source)
    runProgramSymbolic genVariable is newRegs
runProgramSymbolic genVariable ((IDiv dest source):is) regs = do
    let rhs = fetchRegister regs source
    let newRegs = updateRegister regs dest $ Div (fetchRegister regs (Reg dest)) rhs
    runProgramSymbolic genVariable is newRegs
runProgramSymbolic genVariable ((IMod dest source):is) regs = do
    let lhs = fetchRegister regs (Reg dest)
    let rhs = fetchRegister regs source
    let newRegs = updateRegister regs dest $ Div lhs rhs
    runProgramSymbolic genVariable is newRegs
runProgramSymbolic genVariable ((IEql dest source):is) regs = do
    let newRegs = updateRegister regs dest $ Eql (fetchRegister regs (Reg dest)) (fetchRegister regs source)
    runProgramSymbolic genVariable is newRegs

numberToZ3 :: Number -> String
numberToZ3 (Variable v) = [v]
numberToZ3 (Number n) = "(_ bv" <> show n <> " 64)"
numberToZ3 (Div a b) = "(bvudiv " <> numberToZ3 a <> " " <> numberToZ3 b <> ")"
--numberToZ3 (Mod n) = "(bvurem " <> numberToZ3 n <> " (_ bv8 64))"

constraintToZ3 :: Constraint -> String
constraintToZ3 (Constraint Eq a b) = "(assert (= " <> numberToZ3 a <> " " <> numberToZ3 (Number b) <> "))"
constraintToZ3 (Constraint Neq a b) = "(assert (not (= " <> numberToZ3 a <> " " <> numberToZ3 (Number b) <> ")))"

mkZ3 :: [Constraint] -> String
mkZ3 cs = "(declare-const a (_ BitVec 64))\n" <> unlines (fmap constraintToZ3 cs) <> "(assert (bvult a #x00008ec6fee754fb))\n(check-sat)\n(get-model)" -- domain specific constraint to find smallest number
