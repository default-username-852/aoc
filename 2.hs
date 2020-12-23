main :: IO ()
main = do
    indata <- readFile "2.in"
    putStrLn $ show $ sum $ map (part1 . parseIndata) $ lines indata
    putStrLn $ show $ sum $ map (part2 . parseIndata) $ lines indata

parseIndata :: String -> (Int, Int, Char, String)
parseIndata indata = (read fstnum, read sndnum, head letter, password)
    where
        (fstnum, rest) = splitWhen (=='-') indata
        (sndnum, rest') = splitWhen (==' ') rest
        (letter, rest'') = splitWhen (==':') rest'
        password = tail rest''

part1 :: (Int, Int, Char, String) -> Int
part1 (lower, upper, letter, password) = if matching >= lower && matching <= upper then 1 else 0
    where
        matching = length $ filter (==letter) password

part2 :: (Int, Int, Char, String) -> Int
part2 (lower, upper, letter, password)
    | first == letter && second /= letter = 1
    | first /= letter && second == letter = 1
    | otherwise = 0
    where
        first = password !! (lower - 1)
        second = password !! (upper - 1)

splitWhen :: (t -> Bool) -> [t] -> ([t], [t])
splitWhen pred lst
    | null lst = ([], [])
    | pred $ head lst = ([], tail lst)
    | otherwise = (head lst:begin, end)
    where
        (begin, end) = splitWhen pred $ tail lst
