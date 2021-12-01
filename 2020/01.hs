main :: IO ()
main = do
    indata <- readFile "1.in"
    let nums = map read $ words indata
    putStrLn $ show $ part1 nums
    putStrLn $ show $ part2 nums


part1 :: [Int] -> Int
part1 nums = head [x * y | x <- nums, y <- nums, x + y == 2020]

part2 :: [Int] -> Int
part2 nums = head [x * y * z | x <- nums, y <- nums, z <- nums, x + y + z == 2020]
