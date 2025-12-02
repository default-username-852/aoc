module Aoc.Day2(solve) where
import Aoc.Utils(splitWhen)
import Data.List(nub, sort)

type Indata = [(Int, Int)]

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

parseRange :: String -> (Int, Int)
parseRange range = let (a:b:_) = splitWhen (=='-') range in (read a, read b)

parseData :: String -> Indata
parseData input = parseRange <$> splitWhen (==',') input

splitInParts :: Int -> Int -> Int
splitInParts parts num =
    let asStr = show num
        strLen = length asStr
        toTake = strLen `div` parts
    in if toTake == 0 then 1 else read $ take toTake asStr

splitInPartsRoundUp :: Int -> Int -> Int
splitInPartsRoundUp parts num =
    let asStr = show num
        strLen = length asStr
    in read $ take (ceiling $ (fromIntegral strLen :: Rational) / (fromIntegral parts)) asStr

makeID :: Int -> Int -> String
makeID times n = mconcat . take times . repeat $ show n

generateIDs :: Int -> (Int, Int) -> [Int]
generateIDs parts (start, end) =
    let lower = splitInParts parts start
        upper = splitInPartsRoundUp parts end
    in [lower..upper]

idInRange :: (Int, Int) -> String -> Bool
idInRange (lower, upper) id = let parsed = read id in parsed >= lower && parsed <= upper

generateInvalidIDs :: Int -> (Int, Int) -> [String]
generateInvalidIDs parts range = filter (idInRange range) . fmap (makeID parts) $ generateIDs parts range

part1 :: Indata -> Int
part1 indata = sum . fmap read $ concatMap (generateInvalidIDs 2) indata

generateAllInvalidIDs :: (Int, Int) -> [String]
generateAllInvalidIDs range@(_, upper) = mconcat [generateInvalidIDs parts range | parts <- [2..length (show upper)]]

part2 :: Indata -> Int
part2 indata = sum . fmap read . nub . sort $ concatMap (generateAllInvalidIDs) indata
