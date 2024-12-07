{-# LANGUAGE ScopedTypeVariables #-}
module Aoc.Day7(solve) where
import Data.List (uncons)
import Data.Bifunctor (Bifunctor(..))
import Data.Maybe (fromJust)

type Indata = [(Int, [Int])]

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

parseData :: String -> Indata
parseData input = bimap (read . init) (fmap read) . fromJust . uncons . words <$> lines input

possibleEvals :: [Int -> Int -> Int] -> [Int] -> [Int]
possibleEvals _ [] = []
possibleEvals _ [x] = [x]
possibleEvals possibleOperations (x1:x2:xs) = do
    operation <- possibleOperations
    let newX = operation x1 x2
    possibleEvals possibleOperations (newX:xs)

part1 :: Indata -> Int
part1 indata = sum . fmap fst $ filter (\(desired, values) -> desired `elem` possibleEvals [(+), (*)] values) indata

concatNumbers :: Int -> Int -> Int
concatNumbers a b =
    let bSize :: Int = (+1) . floor $ logBase 10 (fromIntegral b :: Double)
    in a * 10 ^ bSize + b

part2 :: Indata -> Int
part2 indata = sum . fmap fst $ filter (\(desired, values) -> desired `elem` possibleEvals [(+), (*), concatNumbers] values) indata
