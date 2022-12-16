import Utils
import Data.List

type Indata = [[Int]]

main :: IO ()
main = do
    indata <- parseData <$> readFile "8.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData = fmap (fmap (read . (:[]))) . lines

part1 :: Indata -> Int
part1 indata = length $ filter (\(x,y) -> isVisible x y indata) matrix
    where
        matrix = [(x,y) | x <- [0..length (head indata) - 1], y <- [0..length indata - 1]]

isVisible :: Int -> Int -> [[Int]] -> Bool
isVisible x y trees = visibleRow || visibleRow' || visibleCol || visibleCol'
    where
        treeHeight = (trees !! y) !! x
        treeRow = trees !! y
        visibleRow = all (<treeHeight) $ take x treeRow
        visibleRow' = all (<treeHeight) $ drop (x + 1) treeRow
        treeCol = transpose trees !! x
        visibleCol = all (<treeHeight) $ take y treeCol
        visibleCol' = all (<treeHeight) $ drop (y + 1) treeCol

scenicScore :: Int -> Int -> [[Int]] -> Int
scenicScore x y trees = visibleRow * visibleRow' * visibleCol * visibleCol'
    where
        treeHeight = (trees !! y) !! x
        treeRow = trees !! y
        countTrees = length . getVisible
        getVisible :: [Int] -> [Int]
        getVisible l = let (h,t) = span (<treeHeight) l in if null t then h else h ++ [head t]
        visibleRow = countTrees . reverse $ take x treeRow
        visibleRow' = countTrees $ drop (x+1) treeRow
        treeCol = transpose trees !! x
        visibleCol = countTrees . reverse $ take y treeCol
        visibleCol' = countTrees $ drop (y+1) treeCol

makeIncreasing :: [Int] -> [Int]
makeIncreasing [] = []
makeIncreasing [x] = [x]
makeIncreasing (x:y:xs)
    | x < y = x:makeIncreasing (y:xs)
    | otherwise = makeIncreasing (x:xs)

part2 :: Indata -> Int
part2 indata = maximum $ fmap (\(x,y) -> scenicScore x y indata) matrix
    where
        matrix = [(x,y) | x <- [0..length (head indata) - 1], y <- [0..length indata - 1]]
