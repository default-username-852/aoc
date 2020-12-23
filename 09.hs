import Data.List
import Utils

main :: IO ()
main = do
    indata <- readFile "9.in"
    print $ part1 indata
    print $ part2 indata

part1 :: String -> Int
part1 = last . head . filter (not . valid) . windows 26 . map read . lines

part2 :: String -> Int
part2 indata = let sequence = head $ filter ((== part1 indata)  . sum) $ filter ((>=2) . length) $ sublists $ map read $ lines indata in maximum sequence + minimum sequence

valid :: [Int] -> Bool
valid nums = let (preamble, num:_) = splitAt 25 nums in any (==num) $ concat [(+) first <$> rest | (first:rest) <- init $ tails preamble]
