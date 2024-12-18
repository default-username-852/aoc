{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Aoc.Day18(solve) where
import Linear
import Aoc.Utils (splitWhen)
import qualified Data.Set as S
import Data.Array (Ix(inRange))
import Algorithm.Search (dijkstra)
import Data.Maybe (fromJust, isJust)

type Indata = [V2 Int]

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    putStrLn $ part2 parsed

parseData :: String -> Indata
parseData input = (\(a:b:_) -> V2 a b) . fmap read . splitWhen (==',') <$> lines input

mapSize :: Int
mapSize = 70

neighbours :: V2 Int -> [V2 Int]
neighbours pos = fmap (pos+) [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]

nextMoves :: S.Set (V2 Int) -> V2 Int -> [V2 Int]
nextMoves blocked pos = filter (inRange (V2 0 0, V2 mapSize mapSize)) . filter (not . (`S.member` blocked)) $ neighbours pos

part1 :: Indata -> Int
part1 indata = fst . fromJust $ dijkstra (nextMoves blocked) (\_ _ -> 1) (== V2 mapSize mapSize) (V2 0 0)
    where
        blocked = S.fromList $ take 1024 indata

findFirstBlockingByte :: Indata -> Int -> Int -> V2 Int
findFirstBlockingByte coords lower upper
    | lower + 1 == upper = coords !! lower
    | otherwise = if isJust $ dijkstra (nextMoves blocked) (const (const (1 :: Int))) (== V2 mapSize mapSize) (V2 0 0)
        then findFirstBlockingByte coords mid upper
        else findFirstBlockingByte coords lower mid
    where
        mid = (lower + upper) `div` 2
        blocked = S.fromList $ take mid coords

part2 :: Indata -> String
part2 indata = let (V2 x y) = findFirstBlockingByte indata 0 (length indata) in show x <> "," <> show y
