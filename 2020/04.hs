import Data.List as L
import Text.Regex.Base
import Text.Regex.PCRE ((=~), (=~~))

main :: IO ()
main = do
    indata <- readFile "4.in"
    print $ part1 indata
    print $ part2 indata


part1 :: String -> Int
part1 indata = sum [1 | passport <- pps, let checks = map L.isInfixOf keys, all id $ checks <*> [passport]]
    where
        keys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
        pps = passports indata

part2 :: String -> Int
part2 indata = sum [1 | passport <- pps, validPassport passport]
    where
        pps = passports indata

passports :: String -> [String]
passports = map (intercalate " ") . splitWhen (=="") . lines

validPassport :: String -> Bool
validPassport passport = validBirthYear && validIssueYear && validExpirationYear && validHeight && validHairColor && validEyeColor && validPID
    where
        validBirthYear = case passport =~~ "byr:(\\d{4})" :: Maybe (String, String, String, [String]) of
            Just (_, _, _, (num:_)) -> read num <= 2002 && read num >= 1920
            _ -> False
        validIssueYear = case passport =~~ "iyr:(\\d{4})" :: Maybe (String, String, String, [String]) of
            Just (_, _, _, (num:_)) -> read num <= 2020 && read num >= 2010
            _ -> False
        validExpirationYear = case passport =~~ "eyr:(\\d{4})" :: Maybe (String, String, String, [String]) of
            Just (_, _, _, (num:_)) -> read num <= 2030 && read num >= 2020
            _ -> False
        validHeight = case passport =~~ "hgt:(\\d*)(cm|in)" :: Maybe (String, String, String, [String]) of
            Just (_, _, _, (num:unit:_)) -> if unit == "cm" then read num <= 193 && read num >= 150 else read num <= 76 && read num >= 59
            _ -> False
        validHairColor :: Bool
        validHairColor = passport =~ "hcl:#[\\da-f]{6}"
        validEyeColor :: Bool
        validEyeColor = passport =~ "ecl:(amb|blu|brn|gry|grn|hzl|oth)"
        validPID :: Bool
        validPID = passport =~ "pid:\\d{9}($| )"


splitWhen :: (t -> Bool) -> [t] -> [[t]]
splitWhen pred lst
    | null lst = [[]]
    | pred $ head lst = []:(splitWhen pred $ tail lst)
    | otherwise = (head lst:begin):end
    where
        (begin:end) = splitWhen pred $ tail lst
