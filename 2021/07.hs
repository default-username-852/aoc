import Utils

type Indata = [Int]

main :: IO ()
main = do
    indata <- parseData <$> readFile "07.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData = fmap read . splitWhen (==',') . head . lines

part1 :: Indata -> Int
part1 indata = minimum [sum $ fmap (\e -> abs $ e - x) indata | x <- [minimum indata..maximum indata]]

part2 :: Indata -> Int
part2 indata = minimum [sum $ fmap (\e -> let n = abs $ e - x in n * (n+1) `div` 2) indata | x <- [minimum indata..maximum indata]]
