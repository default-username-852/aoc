import Utils

type PairRange = ((Int, Int), (Int, Int))
type Indata = [PairRange]

main :: IO ()
main = do
    indata <- parseData <$> readFile "4.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData indata = fmap parseLine $ lines indata
    where
        parseLine :: String -> PairRange
        parseLine l = let (a:b:_) = splitWhen (==',') l in (parseRange a, parseRange b)
        parseRange s = let (a:b:_) = splitWhen (=='-') s in (read a, read b)

part1 :: Indata -> Int
part1 = length . filter containsEachother

containsEachother :: PairRange -> Bool
containsEachother ((a,b),(x,y)) = (a <= x && b >= y) || (x <= a && y >= b)

overlaps :: PairRange -> Bool
overlaps ((a,b),(x,y)) = not (b < x || a > y)

part2 :: Indata -> Int
part2 = length . filter overlaps
