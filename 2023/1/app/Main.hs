import Utils
import Data.Char
import Debug.Trace (traceShowId)

type Indata = [String]

main :: IO ()
main = do
    indata <- parseData <$> getContents
    --print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData = lines

part1 :: Indata -> Int
part1 indata = sum $ fmap calibrationNumber indata

calibrationNumber :: String -> Int
calibrationNumber s =
    let ds = filter isDigit s
    in read [head ds, last ds]

findDigits :: String -> [Int]
findDigits "" = []
findDigits ('e':'i':'g':'h':'t':'w':'o':cs) = 8:2:findDigits cs
findDigits ('e':'i':'g':'h':'t':'h':'r':'e':'e':cs) = 8:3:findDigits cs
findDigits ('s':'e':'v':'e':'n':'i':'n':'e':cs) = 7:9:findDigits cs
findDigits ('t':'w':'o':'n':'e':cs) = 2:1:findDigits cs
findDigits ('o':'n':'e':'i':'g':'h':'t':cs) = 1:8:findDigits cs
findDigits ('t':'h':'r':'e':'e':'i':'g':'h':'t':cs) = 3:8:findDigits cs
findDigits ('o':'n':'e':cs) = 1:findDigits cs
findDigits ('t':'w':'o':cs) = 2:findDigits cs
findDigits ('t':'h':'r':'e':'e':cs) = 3:findDigits cs
findDigits ('f':'o':'u':'r':cs) = 4:findDigits cs
findDigits ('f':'i':'v':'e':cs) = 5:findDigits cs
findDigits ('s':'i':'x':cs) = 6:findDigits cs
findDigits ('s':'e':'v':'e':'n':cs) = 7:findDigits cs
findDigits ('e':'i':'g':'h':'t':cs) = 8:findDigits cs
findDigits ('n':'i':'n':'e':cs) = 9:findDigits cs
findDigits (c:cs)
    | isDigit c = let ds = findDigits cs in read [c]:ds
    | otherwise = findDigits cs

part2 :: Indata -> Int
part2 indata = sum $ fmap ((\ds->10 * head ds + last ds) . findDigits) indata
