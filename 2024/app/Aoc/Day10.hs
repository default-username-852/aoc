module Aoc.Day10(solve) where
import Data.Array (Array, array, bounds, Ix (inRange), (!), assocs)
import Linear
import Prelude hiding (map)
import qualified Data.Set as S

type Indata = Array (V2 Int) Int

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

parseData :: String -> Indata
parseData input =
    let annotated = concatMap (\(y,row) -> (\(x,c) -> (V2 x y, read [c])) <$> zip [0 :: Int ..] row) . zip [0..] $ lines input
    in array (V2 0 0, maximum $ fmap fst annotated) annotated

ascend :: Indata -> V2 Int -> [V2 Int]
ascend map pos =
    let neighbours = filter ((==height) . flip (-) 1 . (map!)) . filter (inRange b) $ (+pos) <$> [V2 (-1) 0, V2 1 0, V2 0 (-1), V2 0 1]
        b = bounds map
        height = map ! pos
    in neighbours

reachablePeaks :: Indata -> V2 Int -> S.Set (V2 Int)
reachablePeaks map pos
    | map ! pos == 9 = S.singleton pos
    | otherwise = mconcat . fmap (reachablePeaks map) $ ascend map pos

part1 :: Indata -> Int
part1 indata = sum . fmap (S.size . reachablePeaks indata . fst) . filter ((==0) . snd) $ assocs indata

scoreTile :: Indata -> V2 Int -> Int
scoreTile map pos
    | map ! pos == 9 = 1
    | otherwise = sum . fmap (scoreTile map) $ ascend map pos

part2 :: Indata -> Int
part2 indata = sum . fmap (scoreTile indata . fst) . filter ((==0) . snd) $ assocs indata
