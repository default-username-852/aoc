
main :: IO ()
main = do
    indata <- readFile "3.in"
    print $ part1 $ lines indata
    print $ part2 $ lines indata

part1 :: [String] -> Int
part1 rows = sum [1 | (row, pos) <- zip rows' [0..], row !! (3 * pos) == '#']
    where
        rows' = map cycle rows

part2 :: [String] -> Int
part2 rows = sum1 * sum2 * sum3 * sum4 * sum5
    where
        rows' = map cycle rows
        sum1 = sum [1 | (row, pos) <- zip rows' [0..], row !! (1 * pos) == '#']
        sum2 = sum [1 | (row, pos) <- zip rows' [0..], row !! (3 * pos) == '#']
        sum3 = sum [1 | (row, pos) <- zip rows' [0..], row !! (5 * pos) == '#']
        sum4 = sum [1 | (row, pos) <- zip rows' [0..], row !! (7 * pos) == '#']
        sum5 = sum [1 | (row, pos) <- zip rows' [0..], pos `mod` 2 == 0, row !! (div pos 2) == '#']

