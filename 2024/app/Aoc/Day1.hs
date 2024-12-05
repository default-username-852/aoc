{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Aoc.Day1(solve) where
import qualified Data.Map.Strict as M
import Data.List (sort)

type Indata = [(Int, Int)]

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

parseData :: String -> Indata
parseData input = (\(a:b:_) -> (read a, read b)) . words <$> lines input

part1 :: Indata -> Int
part1 indata = let (lefts, rights) = unzip indata
    in sum $ zipWith (\a b -> abs (a - b)) (sort lefts) (sort rights)

part2 :: Indata -> Int
part2 indata = let occurences = foldr ((\e acc -> M.insertWith (+) e (1 :: Int) acc) . snd) M.empty indata
    in sum $ fmap (\v -> fst v * M.findWithDefault 0 (fst v) occurences) indata
