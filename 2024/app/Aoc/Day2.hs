module Aoc.Day2(solve) where
import qualified Aoc.Utils as Utils

type Indata = [[Int]]

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

parseData :: String -> Indata
parseData inData = fmap read . words <$> lines inData

increasing :: [Int] -> Bool
increasing = pairProperty (<=)

decreasing :: [Int] -> Bool
decreasing = pairProperty (>=)

atMost3Diff :: [Int] -> Bool
atMost3Diff = pairProperty (\a b -> let diff = abs (a - b) in diff >= 1 && diff <= 3)

pairProperty :: (Int -> Int -> Bool) -> [Int] -> Bool
pairProperty prop xs = and $ zipWith prop xs $ drop 1 xs

safe :: [Int] -> Bool
safe report = (increasing report || decreasing report) && atMost3Diff report

part1 :: Indata -> Int
part1 indata = length $ filter safe indata

removeOne :: [Int] -> [[Int]]
removeOne report = filter ((==(length report - 1)) . length) $ Utils.subsets report

part2 :: Indata -> Int
part2 indata = length $ filter (\report -> any safe . (report:) $ removeOne report) indata
