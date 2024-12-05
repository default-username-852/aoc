module Aoc.Day4(solve) where
import Data.Array ( Ix(range, inRange), Array, (!), array, bounds )

type Indata = Array (Int, Int) Char

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

parseData :: String -> Indata
parseData indata =
    let ls = lines indata
    in array ((1, 1), (length ls, length $ head ls))
        [((row, col), c) | (ws, row) <- zip ls [1..], (c, col) <- zip ws [1..]]

hasXmas :: Indata -> (Int, Int) -> Int
hasXmas matrix (row, col) =
    let directions = [(dy, dx) | dy <- [-1..1], dx <- [-1..1], dy /= 0 || dx /= 0]
        offsets = fmap (\(dy, dx) -> [(dy*delta + row, dx*delta + col) | delta <- [0..3]]) directions
        b = bounds matrix
    in length $ filter (\dir -> fmap (matrix !) dir == "XMAS") $ filter (all (inRange b)) offsets

part1 :: Indata -> Int
part1 indata = sum $ fmap (hasXmas indata) $ range $ bounds indata

hasMas :: Indata -> (Int, Int) -> Bool
hasMas matrix (row, col) =
    let firstSlope = [[(start*delta+row, start*delta+col) | delta <- [-1..1]] | start <- [-1,1]]
        secondSlope = [[(start*delta+row,-start*delta+col) | delta <- [-1..1]] | start <- [-1,1]]
    in ((all (inRange (bounds matrix)) [(dx+row,dy+col) | dx <- [-1,1], dy <- [-1,1]])
        && (
            any ((=="MAS") . fmap (matrix !)) firstSlope
            && any ((=="MAS") . fmap (matrix !)) secondSlope))

part2 :: Indata -> Int
part2 indata = length . filter (hasMas indata) . range $ bounds indata
