import Utils
import qualified Data.Map as M

type Point = (Int, Int)
type Indata = [(Point, Point)]

main :: IO ()
main = do
    indata <- parseData <$> readFile "05.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData indata = fmap parseLine $ lines indata
    where
        parseLine l = let (a:b:_) = splitWhen (=='-') l in (parsePair a, parsePair $ tail b)
        parsePair p = let (a:b:_) = splitWhen (==',') p in (read a, read b)

aligned :: (Point, Point) -> Bool
aligned ((x1,y1),(x2,y2)) = x1 == x2 || y1 == y2

covers :: (Point, Point) -> [Point]
covers ((x1,y1),(x2,y2)) = [(x,y) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2]]

covers' :: (Point, Point) -> [Point]
covers' i@((x1,y1),(x2,y2))
    | aligned i = covers i
    | otherwise = zip [x1,x1+signum (x2-x1)..x2] [y1,y1+signum (y2-y1)..y2]

part1 :: Indata -> Int
part1 indata = M.size . M.filter (>=2) . foldl (\m e -> M.insertWith (+) e 1 m) M.empty $ covers =<< filter aligned indata

part2 :: Indata -> Int
part2 indata = M.size . M.filter (>=2) . foldl (\m e -> M.insertWith (+) e 1 m) M.empty $ covers' =<< indata
