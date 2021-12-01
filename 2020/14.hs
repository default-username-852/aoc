import Text.Regex.Base
import Text.Regex.PCRE ((=~), (=~~))
import qualified Data.Map as M
import Data.Maybe
import Utils
import Data.Bits
import Control.Monad

data Instruction = Mask (Integer -> Integer) | MemSet Int Integer

main :: IO ()
main = do
    indata <- readFile "14.in"
    print $ part1 indata
    print $ part2 indata

part1 :: String -> Integer
part1 indata = sum $ M.elems $ snd $ foldl runInstruction (id, M.empty) $ parseData indata

parseData :: String -> [Instruction]
parseData = map parseInstruction . lines

parseInstruction :: String -> Instruction
parseInstruction line
    | line =~ "mask = ([01X]{36})" = let (_, _, _, (mask:_)) = line =~ "mask = ([01X]{36})" :: (String, String, String, [String]) in Mask $ foldl (.) id $ zipWith maskFunc mask [35,34..0]
    | otherwise = let (_, _, _, (pos:val:_)) = line =~ "mem\\[(\\d+)\\] = (\\d+)" :: (String, String, String, [String]) in MemSet (read pos) (read val)
    where
        maskFunc 'X' _ = id
        maskFunc '1' n = flip setBit n
        maskFunc '0' n = flip clearBit n

runInstruction :: (Integer -> Integer, M.Map Int Integer) -> Instruction -> (Integer -> Integer, M.Map Int Integer)
runInstruction (mask, mem) (Mask mask') = (mask', mem)
runInstruction (mask, mem) (MemSet pos val) = (mask, M.insert pos (mask val) mem)

data Instruction' = Mask' (Integer -> [Integer]) | MemSet' Integer Integer

part2 :: String -> Integer
part2 indata = sum $ M.elems $ snd $ foldl runInstruction' (return, M.empty) $ parseData' indata

parseData' :: String -> [Instruction']
parseData' = map parseInstruction' . lines

parseInstruction' :: String -> Instruction'
parseInstruction' line
    | line =~ "mask = ([01X]{36})" = let (_, _, _, (mask:_)) = line =~ "mask = ([01X]{36})" :: (String, String, String, [String]) in Mask' $ foldl (flip (<=<)) return $ zipWith maskFunc mask [35,34..0]
    | otherwise = let (_, _, _, (pos:val:_)) = line =~ "mem\\[(\\d+)\\] = (\\d+)" :: (String, String, String, [String]) in MemSet' (read pos) (read val)
    where
        maskFunc 'X' n = \num -> [setBit num n, clearBit num n]
        maskFunc '1' n = return . flip setBit n
        maskFunc '0' _ = return

runInstruction' :: (Integer -> [Integer], M.Map Integer Integer) -> Instruction' -> (Integer -> [Integer], M.Map Integer Integer)
runInstruction' (mask, mem) (Mask' mask') = (mask', mem)
runInstruction' (mask, mem) (MemSet' pos val) = (mask, foldl (flip $ flip M.insert val) mem (mask pos))
