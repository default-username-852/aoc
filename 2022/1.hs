import Utils
import Data.List

type Indata = [[Int]]

main :: IO ()
main = do
    indata <- parseData <$> readFile "1.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData d = fmap read <$> splitWhen (=="") (lines d)

part1 :: Indata -> Int
part1 = maximum . fmap sum

part2 :: Indata -> Int
part2 = sum . take 3 . reverse . sort . fmap sum
