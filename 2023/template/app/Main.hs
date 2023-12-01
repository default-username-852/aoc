import Utils

type Indata = String

main :: IO ()
main = do
    indata <- parseData <$> readFile ".in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData = id

part1 :: Indata -> Int
part1 indata = 0

part2 :: Indata -> Int
part2 indata = 0
