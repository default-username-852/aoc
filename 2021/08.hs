{-# LANGUAGE ScopedTypeVariables #-}

import Utils
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Control.Monad

type Indata = [([String], [String])]

main :: IO ()
main = do
    indata <- parseData <$> readFile "08.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData indata = fmap (\l -> let (a:b:_) = splitWhen (=="|") $ words l in (a,b)) $ lines indata

part1 :: Indata -> Int
part1 indata = sum $ fmap (\(_, nums) -> length $ filter ((`elem` [2,3,4,7]) . length) nums) indata

part2 :: Indata -> Int
part2 indata = sum $ fmap solveLine indata
    where
        solveLine :: ([String], [String]) -> Int
        solveLine (cs, vs) = let (sol:[]) = solveDigit cs in foldl (\acc v -> acc * 10 + v) 0 $ fmap ((digits M.!) . S.fromList . fmap (sol M.!)) vs

data Segment = A | B | C | D | E | F | G deriving (Show, Eq, Ord)

digits :: M.Map (S.Set Segment) Int
digits = M.fromList . flip zip [0..] $ fmap S.fromList
         [ [A,B,C,D,E,F]
         , [B,C]
         , [A,B,D,E,G]
         , [A,B,C,D,G]
         , [B,C,F,G]
         , [A,C,D,F,G]
         , [A,C,D,E,F,G]
         , [A,B,C]
         , [A,B,C,D,E,F,G]
         , [A,B,C,D,F,G]
         ]

uniqueDigit :: String -> Bool
uniqueDigit = (`elem` [2,3,4,7]) . length

allMappings :: [M.Map Char Segment]
allMappings = fmap (M.fromList . flip zip [A,B,C,D,E,F,G]) $ permutations "abcdefg"

solveDigit :: [String] -> [M.Map Char Segment]
solveDigit activations = do
    mapping <- allMappings
    let mapped = fmap (S.fromList . fmap (mapping M.!)) activations
    guard $ all (flip elem digits') mapped
    return mapping
    where
        digits' = M.keysSet digits
