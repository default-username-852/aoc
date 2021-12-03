import Utils
import Data.List (transpose)
import Data.Function (on)

type Indata = [String]

main :: IO ()
main = do
    indata <- parseData <$> readFile "03.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData = lines

part1 :: Indata -> Int
part1 indata = on (*) binaryToDec masked (fmap (\c -> if c == '1' then '0' else '1') masked)
    where
        masked = fmap (mostCommonDigit False '0') $ transpose indata

part2 :: Indata -> Int
part2 indata = on (*) (binaryToDec . head . snd . head . snd . break ((==1) . length . snd)) (iterate (iterateList True '0') (0, indata)) (iterate (iterateList False '1') (0, indata))
    where
        iterateList :: Bool -> Char -> (Int, [String]) -> (Int, [String])
        iterateList flip def (pos, strs) = (pos + 1, let keep = mostCommonDigit flip def $ transpose strs !! pos in filter ((==keep) . (!! pos)) strs)

binaryToDec :: String -> Int
binaryToDec numbers = foldl (\i c -> if c == '1' then 2 * i + 1 else 2 * i) 0 numbers

mostCommonDigit :: Bool -> Char -> String -> Char
mostCommonDigit flip def string = 
    let (zs, os) = foldl (\(zs,os) c -> if c == '0' then (zs + 1, os) else (zs, os + 1)) (0,0) string 
    in case zs `compare` os of
        LT -> if flip then '0' else '1'
        EQ -> def
        GT -> if flip then '1' else '0'
