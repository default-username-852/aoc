module Aoc.Day16(solve) where
import Data.Array (Array, array, (!), assocs)
import Linear
import Prelude hiding (map)
import Algorithm.Search (dijkstraAssoc)
import Data.Maybe (fromJust)
import Data.Foldable (find)

type Indata = Array (V2 Int) MapTile

data MapTile = Empty | Wall | Start | Goal deriving (Show, Eq)

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

parseData :: String -> Indata
parseData input =
    let annotatedMap = concatMap (\(y, row) -> (\(x, c) -> (V2 x y, toTile c)) <$> zip [0..] row) . zip [0..] $ lines input
    in array (V2 0 0, maximum $ fmap fst annotatedMap) annotatedMap
    where
        toTile '#' = Wall
        toTile '.' = Empty
        toTile 'S' = Start
        toTile 'E' = Goal
        toTile _ = error "invalid char in input"

rotateRight :: V2 Int -> V2 Int
rotateRight v = V2 (V2 0 (-1)) (V2 1 0) !* v

rotateLeft :: V2 Int -> V2 Int
rotateLeft v = V2 (V2 0 1) (V2 (-1) 0) !* v

type ReindeerState = (V2 Int, V2 Int)

legalReindeerMoves :: Indata -> ReindeerState -> [(ReindeerState, Int)]
legalReindeerMoves map (pos, dir) = [((pos, rotateLeft dir), 1000), ((pos, rotateRight dir), 1000)]
    <> if map ! (pos + dir) /= Wall then [((pos + dir, dir), 1)] else []

part1 :: Indata -> Int
part1 indata = fst . fromJust $ dijkstraAssoc (legalReindeerMoves indata) ((==Goal) . (indata!) . fst)
        (fst . fromJust . find ((==Start) . snd) $ assocs indata, V2 1 0)

costWhenGoingPast :: Int -> Indata -> V2 Int -> Int
costWhenGoingPast costCeiling map tile =
    let startState = (fst . fromJust . find ((==Start) . snd) $ assocs map, V2 1 0)
        (firstCost, firstPath) = fromJust $ dijkstraAssoc (legalReindeerMoves map) ((==tile) . fst) startState
        (secondCost, _) = fromJust $ dijkstraAssoc (legalReindeerMoves map) ((==Goal) . (map!) . fst) (last firstPath)
    in if firstCost > costCeiling then maxBound else firstCost + secondCost

part2 :: Indata -> Int
part2 indata =
    let bestCost = part1 indata
        acceptableTiles = filter ((==bestCost) . costWhenGoingPast bestCost indata) . fmap fst . filter ((==Empty) . snd) $ assocs indata
    in length acceptableTiles + 2
