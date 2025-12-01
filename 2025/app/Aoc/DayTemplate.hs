module Aoc.Day(solve) where

type Indata = String

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

parseData :: String -> Indata
parseData input = input

part1 :: Indata -> Int
part1 indata = 0

part2 :: Indata -> Int
part2 indata = 0
