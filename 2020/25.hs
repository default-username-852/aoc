import Utils
import Data.List

main :: IO ()
main = print part1 >> print part2

part1 :: Integer
part1 = transform card doorLoop
    where
        doorLoop = length $ takeWhile (/=door) $ iterate (flip transformStep 7) 1

part2 :: Int
part2 = 0

door :: Integer
door = 14222596

card :: Integer
card = 4057428

transformStep :: Integer -> Integer -> Integer
transformStep num subj = (num * subj) `mod` 20201227

transform :: Integer -> Int -> Integer
transform subj loop = (!! loop) $ iterate (flip transformStep subj) 1
