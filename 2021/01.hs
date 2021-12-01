import Utils

main :: IO ()
main = do
    indata <- fmap read . lines <$> readFile "01.in"
    print $ part1 indata
    print $ part2 indata

part1 :: [Int] -> Int
part1 indata = length . filter (\(a:b:_) -> a < b) $ windows 2 indata

part2 :: [Int] -> Int
part2 indata = length . filter (\(a:b:_) -> a < b) . windows 2 . fmap sum $ windows 3 indata
