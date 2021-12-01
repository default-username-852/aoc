import Utils

main :: IO ()
main = do
    indata <- readFile "13.in"
    print $ part1 indata
    print $ part2 indata

part1 :: String -> Int
part1 indata = (time' - time) * busID
    where
        (time, busses) = parseData indata
        (time', busID) = minimum $ map (\busID -> ((ceiling (fromIntegral time / fromIntegral busID)) * busID, busID)) busses

parseData :: String -> (Int, [Int])
parseData indata = let line1:line2:_ = lines indata in (read line1, map read $ filter (/="x") $ splitWhen (==',') line2)

part2 :: String -> Integer
part2 = chinese . parseData'

parseData' :: String -> [(Integer, Integer)]
parseData' indata = let _:line:_ = lines indata in map (\(a, n) -> (negate a, read n)) $ filter ((/="x") . snd) $ zip [0..] $ splitWhen (==',') line

gcd' :: Integer -> Integer -> (Integer, Integer, Integer)
gcd' 0 b = (b, 0, 1)
gcd' a b = let (divisor, x, y) = gcd' (b `mod` a) a in (divisor, y - (b `div` a) * x, x)

chinese :: [(Integer, Integer)] -> Integer
chinese eqs = flip mod ns $ sum $ map (\(a, n) -> let (_, r, s) = gcd' n (ns `div` n) in a * s * (ns `div` n)) eqs
    where
        ns = foldr (*) 1 $ map snd eqs
