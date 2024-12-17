{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Aoc.Day17(solve) where
import Data.Array (Array, listArray, bounds, (!), elems)
import Aoc.Utils (splitWhen)
import Data.Bits (Bits(..))
import Data.List (intercalate)

type Indata = (Array Int Int, Int, Int, Int)

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    putStrLn $ part1 parsed
    putStrLn $ part2 parsed

parseData :: String -> Indata
parseData input =
    let (regA:regB:regC:_:program:_) = lines input
        pProgram = fmap read . splitWhen (==',') $ words program !! 1
        extractReg r = read $ words r !! 2
    in (listArray (0, length pProgram - 1) pProgram, extractReg regA, extractReg regB, extractReg regC)

comboOperand :: (Array Int Int, Int, Int, Int, Int) -> Int -> Int
comboOperand (_, regA, regB, regC, _) val
    | val >= 0 && val <= 3 = val
    | val == 4 = regA
    | val == 5 = regB
    | val == 6 = regC
    | otherwise = error "invalid combo operand"

runProgram :: (Array Int Int, Int, Int, Int, Int) -> [Int] -> [Int]
runProgram state@(program, regA, regB, regC, programCounter) output =
    if programCounter >= snd (bounds program) then reverse output
    else case program ! programCounter of
            0 -> runProgram (program, regA `div` (2 ^ comboOperand state operand), regB, regC, programCounter + 2) output
            1 -> runProgram (program, regA, regB `xor` operand, regC, programCounter + 2) output
            2 -> runProgram (program, regA, comboOperand state operand `mod` 8, regC, programCounter + 2) output
            3 -> runProgram (program, regA, regB, regC, if regA == 0 then programCounter + 2 else operand) output
            4 -> runProgram (program, regA, regB `xor` regC, regC, programCounter + 2) output
            5 -> runProgram (program, regA, regB, regC, programCounter + 2) (comboOperand state operand `mod` 8 : output)
            6 -> runProgram (program, regA, regA `div` (2 ^ comboOperand state operand), regC, programCounter + 2) output
            7 -> runProgram (program, regA, regB, regA `div` (2 ^ comboOperand state operand), programCounter + 2) output
            _ -> error "invalid op"
    where
        operand = program ! (programCounter + 1)

part1 :: Indata -> String
part1 (p, a, b, c) = intercalate "," . fmap show $ runProgram (p, a, b, c, 0) []

data Number
    = Variable
    | Number Int
    | Div Number Number
    | Xor Number Number
    | Mod Number -- mod by 8
    | Pow Number -- 2^the number

instance Show Number where
    show :: Number -> String
    show Variable = "a"
    show (Number n) = show n
    show (Div a b) = "(" <> show a <> " / " <> show b <> ")"
    show (Xor a b) = "(" <> show a <> " xor " <> show b <> ")"
    show (Mod n) = "(" <> show n <> " % 8)"
    show (Pow n) = "(2 ^ " <> show n <> ")"

comboOperandSymbolic :: (Array Int Int, Number, Number, Number, Int) -> Int -> Number
comboOperandSymbolic (_, regA, regB, regC, _) val
    | val >= 0 && val <= 3 = Number val
    | val == 4 = regA
    | val == 5 = regB
    | val == 6 = regC
    | otherwise = error "invalid combo operand"

data Constraint
    = Constraint ConstraintKind Number Int

data ConstraintKind = Eq | Neq

instance Show Constraint where
    show (Constraint kind a b) = show a <> " " <> show kind <> " " <> show b

instance Show ConstraintKind where
    show Eq = "=="
    show Neq = "/="

runProgramSymbolic :: (Array Int Int, Number, Number, Number, Int) -> ([Constraint], [Int]) -> [[Constraint]]
runProgramSymbolic state@(program, regA, regB, regC, programCounter) (output, desired)
  | programCounter >= snd (bounds program) = if null desired then pure $ reverse output else []
--  | length (output) >= 50 = []
  | otherwise = case program ! programCounter of
            0 -> runProgramSymbolic (program, regA `Div` Pow (comboOperandSymbolic state operand), regB, regC, programCounter + 2) (output, desired)
            1 -> runProgramSymbolic (program, regA, regB `Xor` Number operand, regC, programCounter + 2) (output, desired)
            2 -> runProgramSymbolic (program, regA, Mod $ comboOperandSymbolic state operand, regC, programCounter + 2) (output, desired)
            3 ->   runProgramSymbolic (program, regA, regB, regC, programCounter + 2) (Constraint Eq regA 0:output, desired)
                <> runProgramSymbolic (program, regA, regB, regC, operand) (Constraint Neq regA 0:output, desired)
            4 -> runProgramSymbolic (program, regA, regB `Xor` regC, regC, programCounter + 2) (output, desired)
            5 -> case desired of
                [] -> []
                v:rest -> runProgramSymbolic (program, regA, regB, regC, programCounter + 2)
                            (Constraint Eq (Mod (comboOperandSymbolic state operand)) v :output, rest)
            6 -> runProgramSymbolic (program, regA, regA `Div` Pow (comboOperandSymbolic state operand), regC, programCounter + 2) (output, desired)
            7 -> runProgramSymbolic (program, regA, regB, regA `Div` Pow (comboOperandSymbolic state operand), programCounter + 2) (output, desired)
            _ -> error "invalid op"
  where
      operand = program ! (programCounter + 1)

numberToZ3 :: Number -> String
numberToZ3 Variable = "a"
numberToZ3 (Number n) = "(_ bv" <> show n <> " 64)"
numberToZ3 (Div a b) = "(bvudiv " <> numberToZ3 a <> " " <> numberToZ3 b <> ")"
numberToZ3 (Xor a b) = "(bvxor " <> numberToZ3 a <> " " <> numberToZ3 b <> ")"
numberToZ3 (Mod n) = "(bvurem " <> numberToZ3 n <> " (_ bv8 64))"
numberToZ3 (Pow n) = "(bvshl (_ bv1 64) " <> numberToZ3 n <> ")"

constraintToZ3 :: Constraint -> String
constraintToZ3 (Constraint Eq a b) = "(assert (= " <> numberToZ3 a <> " " <> numberToZ3 (Number b) <> "))"
constraintToZ3 (Constraint Neq a b) = "(assert (not (= " <> numberToZ3 a <> " " <> numberToZ3 (Number b) <> ")))"

mkZ3 :: [Constraint] -> String
mkZ3 cs = "(declare-const a (_ BitVec 64))\n" <> unlines (fmap constraintToZ3 cs) <> "(assert (bvult a #x00008ec6fee754fb))\n(check-sat)\n(get-model)" -- domain specific constraint to find smallest number

part2 :: Indata -> String
part2 (p, _, b, c) =
    let goal = head $ runProgramSymbolic (p, Variable, Number b, Number c, 0) ([], elems p)
    in mkZ3 goal
