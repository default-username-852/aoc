{-# LANGUAGE TupleSections #-}
module Aoc.Day20(solve) where
import Data.Array (Array, array, (!), assocs, bounds, inRange)
import Linear
import Prelude hiding (map)
import Algorithm.Search (dfs)
import Data.Foldable (find, Foldable (foldl'))
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad (guard)

type Indata = Array (V2 Int) MapTile

data MapTile = Empty | Wall | Start | Goal deriving (Show, Eq)

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

parseMapTile :: Char -> MapTile
parseMapTile '.' = Empty
parseMapTile '#' = Wall
parseMapTile 'S' = Start
parseMapTile 'E' = Goal
parseMapTile _ = error "unknown map tile"

parseData :: String -> Indata
parseData input =
    let annotated = concatMap (\(y, row) -> (\(x, c) -> (V2 x y, parseMapTile c)) <$> zip [0..] row) . zip [0..] $ lines input
    in array (V2 0 0, maximum $ fmap fst annotated) annotated

neighbours :: V2 Int -> [V2 Int]
neighbours pos = fmap (+pos) [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]

pathToGoal :: Indata -> [V2 Int]
pathToGoal map = (mapStart:) . fromJust $ dfs (filter (\t -> map ! t /= Wall) .  neighbours) ((==Goal) . (map!)) mapStart
    where
        mapStart = fst . fromJust . find ((==Start) . snd) $ assocs map

evalCheat :: M.Map (V2 Int) Int -> V2 Int -> V2 Int -> Int
evalCheat asMap start end =
    --let asMap = M.fromList $ zip path [0..]
    (asMap M.! end - asMap M.! start) - (sum . abs $ start - end)

neighbours2 :: V2 Int -> [V2 Int]
neighbours2 pos = fmap (+pos) [V2 2 0, V2 (-2) 0, V2 1 (-1), V2 1 1, V2 (-1) 1, V2 (-1) (-1), V2 0 2, V2 0 (-2)]

allCheats :: Indata -> [V2 Int] -> [(V2 Int, V2 Int)]
allCheats map = concatMap (\start -> fmap (start,) . filter ((/=Wall) . (map!)) . filter (inRange (bounds map)) $ neighbours2 start)

part1 :: Indata -> Int
part1 indata =
    let path = pathToGoal indata
        asMap = M.fromList $ zip path [0..]
    in length . filter (>= 100) . fmap (uncurry (evalCheat asMap)) $ allCheats indata path

partitionPath :: [V2 Int] -> M.Map (V2 Int) (S.Set (V2 Int))
partitionPath = foldl' insertPoint M.empty
    where
        insertPoint :: M.Map (V2 Int) (S.Set (V2 Int)) -> V2 Int -> M.Map (V2 Int) (S.Set (V2 Int))
        insertPoint map p = M.insertWith S.union (fmap (`div` 20) p) (S.singleton p) map

closePoints :: M.Map (V2 Int) (S.Set (V2 Int)) -> V2 Int -> [V2 Int]
closePoints blocks p =
    concatMap S.toList . mapMaybe ((`M.lookup` blocks) . (+fmap (`div` 20) p)) $ [V2 dx dy | dx <- [-1..1], dy <- [-1..1]]

allCheats2 :: [V2 Int] -> [(V2 Int, V2 Int)]
allCheats2 path = do
    p1 <- path
    p2 <- closePoints partitioned p1
    guard $ pathIdx p1 < pathIdx p2
    guard . (<= 20) . sum . abs $ p1 - p2
    pure (p1, p2)
    where
        partitioned = partitionPath path
        pathIdx :: V2 Int -> Int
        pathIdx p = (M.! p) . M.fromList $ zip path [0..]

part2 :: Indata -> Int
part2 indata =
    let path = pathToGoal indata
        asMap = M.fromList $ zip path [0..]
    in length . filter (>= 100) . fmap (uncurry (evalCheat asMap)) $ allCheats2 path
