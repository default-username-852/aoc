import Utils
import Data.List

type Indata = String

main :: IO ()
main = do
    indata <- parseData <$> readFile "6.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData = id

part1 :: Indata -> Int
part1 indata = (+4) . length . takeWhile (not . allUnique) $ windows 4 indata

allUnique :: String -> Bool
allUnique s = length s == length (nub s)

part2 :: Indata -> Int
part2 indata = (+14) . length . takeWhile (not . allUnique) $ windows 14 indata
