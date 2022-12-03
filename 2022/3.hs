import Utils
import qualified Data.Set as S
import Data.Bifunctor
import Data.Char

type Indata = [(S.Set Char, S.Set Char)]

main :: IO ()
main = do
    indata <- parseData <$> readFile "3.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData indata = fmap parseLine $ lines indata
    where
        parseLine :: String -> (S.Set Char, S.Set Char)
        parseLine l = bimap S.fromList S.fromList $ splitAt (length l `div` 2) l

part1 :: Indata -> Int
part1 = sum . fmap (sum . fmap itemPriority . S.elems . uncurry S.intersection)

part2 :: Indata -> Int
part2 = sum . fmap (itemPriority . head . S.elems . foldl1 S.intersection) . chunksOf 3 . fmap (uncurry S.union)

itemPriority :: Char -> Int
itemPriority c
    | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
    | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 27
