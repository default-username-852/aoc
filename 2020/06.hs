import qualified Data.Set as S
import Utils

main :: IO ()
main = do
    indata <- readFile "6.in"
    print $ part1 indata
    print $ part2 indata

part1 :: String -> Int
part1 = sum . map (S.size . S.unions) . groups

groups :: String -> [[S.Set Char]]
groups = map (map S.fromList) . splitWhen (=="") . lines

part2 :: String -> Int
part2 = sum . map (S.size . (foldl S.intersection $ S.fromList ['a'..'z'])) . groups
