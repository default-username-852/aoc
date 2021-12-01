import qualified Data.Map as M
import Utils

type Point = (Int, Int)

main :: IO ()
main = do
    indata <- readFile "11.in"
    print $ part1 indata
    print $ part2 indata

part1 :: String -> Int
part1 = stabilize advanceTile . readMap

part2 :: String -> Int
part2 = stabilize advanceTile' . readMap

stabilize :: (M.Map Point Char -> Point -> Char) -> M.Map Point Char -> Int
stabilize advanceFunc = length . filter (=='#') . M.elems . last . last . takeWhile (\(bf:aft:_) -> bf /= aft) . windows 2 . iterate (advanceMap advanceFunc)

readMap :: String -> M.Map Point Char
readMap = M.fromList . concat . map (\(y, xs) -> map (\(x, c) -> ((x, y), c)) xs) . zip [0..] . map (zip [0..]) . lines

advanceMap :: (M.Map Point Char -> Point -> Char) -> M.Map Point Char -> M.Map Point Char
advanceMap advanceFunc initial = M.mapWithKey (\k _ -> advanceFunc initial k) initial

neighbours :: [Point -> Point]
neighbours = [\(x, y) -> (succ x, y), \(x, y) -> (pred x, y), \(x, y) -> (succ x, succ y), \(x, y) -> (pred x, succ y), \(x, y) -> (succ x, pred y), \(x, y) -> (pred x, pred y), \(x, y) -> (x, succ y), \(x, y) -> (x, pred y)]

advanceTile :: M.Map Point Char -> Point -> Char
advanceTile chart on
    | current == 'L' && alive == 0 = '#'
    | current == '#' && alive >= 4 = 'L'
    | otherwise = current
    where
        alive = length $ filter (=='#') $ map (flip (M.findWithDefault '.') chart) $ neighbours <*> [on]
        current = chart M.! on

advanceTile' :: M.Map Point Char -> Point -> Char
advanceTile' chart on
    | current == 'L' && alive == 0 = '#'
    | current == '#' && alive >= 5 = 'L'
    | otherwise = current
    where
        alive = length $ filter (any (=='#')) $ map (takeWhile (\e -> e /= ' ' && e /= 'L')) $ map (map $ flip (M.findWithDefault ' ') chart) $ map tail $ map (flip iterate on) $ neighbours
        current = chart M.! on
