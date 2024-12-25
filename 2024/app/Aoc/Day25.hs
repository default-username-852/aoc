module Aoc.Day25(solve) where
import Data.List (transpose)
import Aoc.Utils (splitWhen)
import Data.Bifunctor (Bifunctor(first, second))
import Control.Monad (guard)

type Indata = [Schematic]

data Schematic = Key [Int] | Lock [Int] deriving Show

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed

parseSchematic :: [String] -> Schematic
parseSchematic rows =
    let t = transpose rows
        isLock = head (head rows) == '#'
    in if isLock
        then Lock $ fmap (length . takeWhile (=='#') . tail) t
        else Key $ fmap (length . takeWhile (=='#') . tail . reverse) t

parseData :: String -> Indata
parseData input = fmap parseSchematic $ splitWhen (=="") $ lines input

partitionSchematics :: [Schematic] -> ([[Int]], [[Int]])
partitionSchematics [] = ([], [])
partitionSchematics (Key k:ss) = first (k:) $ partitionSchematics ss
partitionSchematics (Lock k:ss) = second (k:) $ partitionSchematics ss

overlaps :: [Int] -> [Int] -> Bool
overlaps key lock = any (>5) $ zipWith (+) key lock

part1 :: Indata -> Int
part1 indata = sum $ do
    let (keys, locks) = partitionSchematics indata
    k <- keys
    l <- locks
    guard . not $ overlaps k l
    pure 1
