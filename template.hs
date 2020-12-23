import Utils

main :: IO ()
main = do
    indata <- readFile ".in"
    print $ part1 indata
    print $ part2 indata

part1 :: String -> Int
part1 indata = 0

part2 :: String -> Int
part2 indata = 0
