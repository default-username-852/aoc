module Aoc.Day1(solve) where
import Aoc.Utils(windows)

data Rotation = LeftRot Int | RightRot Int deriving Show

type Indata = [Rotation]

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

parseData :: String -> Indata
parseData = fmap parseRow . lines
    where
        parseRow :: String -> Rotation
        parseRow ('L':cs) = LeftRot $ read cs
        parseRow ('R':cs) = RightRot $ read cs

applyRotation :: Int -> Rotation -> Int
applyRotation n (LeftRot r) = (n - r + 100) `mod` 100
applyRotation n (RightRot r) = (n + r + 100) `mod` 100

part1 :: Indata -> Int
part1 indata = length . filter (==0) . scanl applyRotation 50 $ indata

passedNumbers :: Int -> Int -> [Int]
passedNumbers start end 
    | end == start = []
    | end < start = reverse $ passedNumbers (end-1) (start-1)
    | otherwise = [start - signum (start - end)..end]

applyRotation2 :: Int -> Rotation -> Int
applyRotation2 n (LeftRot r) = (n - r)
applyRotation2 n (RightRot r) = (n + r)

part2 :: Indata -> Int
part2 indata = length . filter ((==0) . (`mod` 100)) . concat . fmap (\(a:b:_) -> passedNumbers a b) . windows 2 . scanl applyRotation2 50 $ indata
