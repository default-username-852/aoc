import qualified Data.Maybe as M
import qualified Data.Either as E
import qualified Data.Set as S
import qualified Data.List as L
import Utils

main :: IO ()
main = do
    indata <- readFile "8.in"
    print $ part1 indata
    print $ part2 indata

part1 :: String -> Int
part1 indata = let (_, _, acc, _) = E.fromRight (S.empty, 0, 0, []) $ last $ takeWhile E.isRight $ iterate ((=<<) runProgram) $ Right (S.empty, 0, 0, parseData indata) in acc

part2 :: String -> Int
part2 indata = head $ do
    possibleSwaps <- [0..length program - 1]
    let program' = [if possibleSwaps == line then (flipCommand command, num) else (command, num) | (line, (command, num)) <- zip [0..] program]
    let (ok, err) = span E.isRight $ iterate ((=<<) runProgram) $ Right (S.empty, 0, 0, program')
    let valid = E.fromLeft False $ head err
    acc <- if valid then let (_,_, acc,_) = E.fromRight (S.empty, 0, 0, []) $ last ok in [acc] else []
    return acc
    where
        program = parseData indata

flipCommand :: String -> String
flipCommand "jmp" = "nop"
flipCommand "nop" = "jmp"
flipCommand s = s

runProgram :: (S.Set Int, Int, Int, [(String, Int)]) -> Either Bool (S.Set Int, Int, Int, [(String, Int)])
runProgram (visited, pc, acc, program)
    | S.member pc visited = Left False
    | pc >= length program = Left True
    | fst (program !! pc) == "acc" = Right (S.insert pc visited, pc + 1, acc + (snd $ program !! pc), program)
    | fst (program !! pc) == "jmp" = Right (S.insert pc visited, pc + (snd $ program !! pc), acc, program)
    | fst (program !! pc) == "nop" = Right (S.insert pc visited, pc + 1, acc, program)

parseData :: String -> [(String, Int)]
parseData indata = do
    line <- lines indata
    let (insn:num:_) = words line
    return (insn, if head num == '+' then read $ tail num else read num)
