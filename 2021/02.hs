import Utils

data Direction = Forw | Up | Down

main :: IO ()
main = do
    indata <- parseData <$> readFile "02.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> [(Direction, Int)]
parseData = fmap ((\(dir:len:_) -> (readDir dir, read len)) . words) . lines
    where
        readDir "forward" = Forw
        readDir "up" = Up
        readDir "down" = Down

part1 :: [(Direction, Int)] -> Int
part1 = uncurry (*) . foldl modifyPos (0, 0)
    where
        modifyPos (x, y) (Down, o) = (x, y + o)
        modifyPos (x, y) (Up, o) = (x, y - o)
        modifyPos (x, y) (Forw, o) = (x + o, y)

part2 :: [(Direction, Int)] -> Int
part2 indata = let (x, y, _) = foldl modifyPos (0, 0, 0) indata in x * y
    where
        modifyPos (x, y, a) (Down, o) = (x, y, a + o)
        modifyPos (x, y, a) (Up, o) = (x, y, a - o)
        modifyPos (x, y, a) (Forw, o) = (x + o, y + a * o, a)
