{-# LANGUAGE TupleSections #-}
module Aoc.Day12(solve) where
import Data.Array
import Linear (V2(..), (!*), (^*))
import qualified Data.Set as S
import Prelude hiding (map)

type Indata = Array (V2 Int) Char

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

parseData :: String -> Indata
parseData input =
    let annotated = concatMap (\(y, row) -> (\(x, c) -> (V2 x y, c)) <$> zip [0..] row) . zip [0..] $ lines input
    in array (V2 0 0, maximum $ fmap fst annotated) annotated

neighboursOf :: V2 Int -> [V2 Int]
neighboursOf pos = fmap (+pos) [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]

fillRegion :: Indata -> V2 Int -> S.Set (V2 Int)
fillRegion map pos = go S.empty pos
    where
        regionColor = map ! pos
        mapBounds = bounds map
        go :: S.Set (V2 Int) -> V2 Int -> S.Set (V2 Int)
        go visited on =
            let next = filter ((==regionColor) . (map!)) . filter (inRange mapBounds) . filter (not . (`S.member` visited)) $ neighboursOf on
                newVisited = S.insert on visited
            in foldr (flip go) newVisited next

findRegions :: Indata -> [S.Set (V2 Int)]
findRegions map = go . S.fromList . range $ bounds map
    where
        go :: S.Set (V2 Int) -> [S.Set (V2 Int)]
        go remaining
            | null remaining = []
            | otherwise = let region = fillRegion map (S.elemAt 0 remaining) in region : go (S.difference remaining region)

perimeterOfRegion :: S.Set (V2 Int) -> Int
perimeterOfRegion region = sum . fmap (length . filter (not . (`S.member` region)) . neighboursOf) $ S.toList region

part1 :: Indata -> Int
part1 indata = sum . fmap (\r -> S.size r * perimeterOfRegion r) $ findRegions indata

boundariesOfRegion :: Indata -> S.Set (V2 Int) -> S.Set (V2 Int, V2 Int)
boundariesOfRegion map region =
    S.fromList . concatMap (\p -> fmap (\n -> (p, n - p)) . filter (\n -> not (inRange mapBounds n) || (map ! n /= regionColor)) $ neighboursOf p) $ S.toList region
    where
        regionColor = map ! S.elemAt 0 region
        mapBounds = bounds map

rotateRight :: V2 Int -> V2 Int
rotateRight v = V2 (V2 0 (-1)) (V2 1 0) !* v

rotateLeft :: V2 Int -> V2 Int
rotateLeft v = V2 (V2 0 1) (V2 (-1) 0) !* v

sidesOfRegion :: Indata -> S.Set (V2 Int) -> Int
sidesOfRegion map region = go $ boundariesOfRegion map region
    where
        go :: S.Set (V2 Int, V2 Int) -> Int
        go boundaries
            | null boundaries = 0
            | otherwise =
                let (considering, out) = S.elemAt 0 boundaries
                    lefts  = takeWhile (`S.member` boundaries) $ [(rotateLeft  out ^* n + considering, out) | n <- [1..]]
                    rights = takeWhile (`S.member` boundaries) $ [(rotateRight out ^* n + considering, out) | n <- [1..]]
                in 1 + go (S.difference boundaries . S.fromList $ (considering, out):lefts ++ rights)

part2 :: Indata -> Int
part2 indata = sum . fmap (\r -> S.size r * sidesOfRegion indata r) $ findRegions indata
