import Data.List as L

main :: IO ()
main = do
    indata <- lines <$> readFile "5.in"
    print $ part1 indata
    print $ part2 indata

part1 :: [String] -> Int
part1 seats = maximum [row * 8 + col | seat <- seats, let (row, col) = seatPos seat]

seatPos :: String -> (Int, Int)
seatPos boardingPass = (row, col)
    where
        (fb, lr) = splitAt 7 boardingPass
        row = fst $ foldl binarySplit (0, 127) (map (=='B') fb)
        col = fst $ foldl binarySplit (0, 7) (map (=='R') lr)

binarySplit :: (Int, Int) -> Bool -> (Int, Int)
binarySplit (lower, upper) choose = if choose then (ceiling (fromIntegral (upper + lower) / 2), upper) else (lower, div (upper + lower) 2)

part2 :: [String] -> Int
part2 seats = 1 + (head $ head $ filter (\(l:r:_) -> r - l == 2) $ windows 2 seats')
    where
        seats' = L.sort [row * 8 + col | seat <- seats, let (row, col) = seatPos seat]

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails
