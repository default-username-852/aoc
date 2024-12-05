{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Aoc.Day3(solve) where
import Text.Regex.TDFA ( (=~) )

type Indata = String

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

parseData :: String -> Indata
parseData = id

mulRegex :: String
mulRegex = "mul\\(([0-9]+),([0-9]+)\\)"

part1 :: Indata -> Int
part1 indata =
    let matches = (indata =~ mulRegex :: [[String]])
        nums = fmap (\(_:a:b:_) -> (read a, read b)) matches
    in  sum $ fmap (uncurry (*)) nums

part2Regex :: String
part2Regex = "mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)"

data Instruction = Mul Int Int | Do | Dont deriving Show

extractMatches :: String -> [Instruction]
extractMatches indata =
    let matches = (indata =~ part2Regex :: [[String]])
        parseMatch :: [String] -> Instruction
        parseMatch ("do()":_) = Do
        parseMatch ("don't()":_) = Dont
        parseMatch (_:a:b:_) = Mul (read a) (read b)
        parseMatch _ = undefined
    in  fmap parseMatch matches

evalInsns :: [Instruction] -> Int
evalInsns insns = snd $ foldl step (True, 0) insns
    where
        step :: (Bool, Int) -> Instruction -> (Bool, Int)
        step (_, val) Do = (True, val)
        step (_, val) Dont = (False, val)
        step (False, val) (Mul _ _) = (False, val)
        step (True, val) (Mul a b) = (True, val + a * b)

part2 :: Indata -> Int
part2 = evalInsns . extractMatches

