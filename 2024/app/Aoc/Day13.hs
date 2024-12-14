{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Aoc.Day13(solve) where
import Linear
import Aoc.Utils (splitWhen)
import Text.Regex.TDFA ((=~))
import Data.Maybe (mapMaybe)

type Indata = [(V2 Int, V2 Int, V2 Int)]

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

extractVector :: String -> V2 Int
extractVector l =
    let (_ :: String,_ :: String,_ :: String,x:y:_) = l =~ "Button .: X\\+([0-9]+), Y\\+([0-9]+)" in V2 (read x) (read y)

extractGoal :: String -> V2 Int
extractGoal l =
    let (_ :: String,_ :: String,_ :: String,x:y:_) = l =~ "Prize: X=([0-9]+), Y=([0-9]+)" in V2 (read x) (read y)

parseData :: String -> Indata
parseData input = fmap (\(v1:v2:goal:_) -> (extractVector v1, extractVector v2, extractGoal goal)) . splitWhen (=="") $ lines input

findSolution :: (V2 Int, V2 Int, V2 Int) -> Maybe Int
findSolution (V2 ax ay, V2 bx by, V2 goalx goaly) =
    let determinant = ax * by - ay * bx
        aPresses = goalx * by - goaly * bx
        bPresses = - goalx * ay + goaly * ax
    in if aPresses `rem` determinant /= 0 || bPresses `rem` determinant /= 0 then Nothing else Just (3 * aPresses `div` determinant + bPresses `div` determinant)

part1 :: Indata -> Int
part1 indata = sum (mapMaybe findSolution indata)

part2 :: Indata -> Int
part2 indata = sum (mapMaybe (findSolution . fmap (+10000000000000)) indata)
