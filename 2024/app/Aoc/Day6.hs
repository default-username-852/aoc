{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Aoc.Day6(solve) where
import qualified Data.Set as S
import Linear (V2(V2), (!*))
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Control.Applicative (Applicative(..))

data Indata = Indata
    { mapUpperBounds :: V2 Int
    , mapObstacles :: S.Set (V2 Int)
    , guardPosition :: V2 Int
    , guardDirection :: V2 Int
    } deriving Show

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

parseData :: String -> Indata
parseData input =
    let tagged = concatMap (\(y, row) -> (\(x, c) -> (V2 x y, c)) <$> zip [(0 :: Int)..] row) (zip [0..] $ lines input)
        obstacles = S.fromList . fmap fst $ filter ((=='#') . snd) tagged
        guardPos = fst . fromJust $ find ((=='^') . snd) tagged
        maxBounds = maximum $ fmap fst tagged
    in Indata {
        mapUpperBounds = maxBounds,
        mapObstacles = obstacles,
        guardPosition = guardPos,
        guardDirection = V2 0 (-1)
    }

rotateRight :: V2 Int -> V2 Int
rotateRight v = V2 (V2 0 (-1)) (V2 1 0) !* v

step :: (Indata, S.Set (V2 Int)) -> S.Set (V2 Int)
step (state, visited) =
    let nextGuardPos = guardPosition state + guardDirection state
    in if nextGuardPos `S.member` mapObstacles state
        then step (state { guardDirection = rotateRight $ guardDirection state }, visited)
    else if any (<0) nextGuardPos || any (<0) (liftA2 (-) (mapUpperBounds state) nextGuardPos)
        then visited
        else step (state { guardPosition = nextGuardPos }, S.insert nextGuardPos visited)

part1 :: Indata -> Int
part1 indata =
    let visited = step (indata, S.singleton (guardPosition indata))
    in S.size visited

loops :: (Indata, S.Set (V2 Int, V2 Int)) -> Bool
loops (state, visited) =
    let nextGuardPos = guardPosition state + guardDirection state
    in if (guardPosition state, guardDirection state) `S.member` visited
        then True
    else if nextGuardPos `S.member` mapObstacles state
        then loops (state { guardDirection = rotateRight $ guardDirection state }, visited)
    else if any (<0) nextGuardPos || any (<0) (liftA2 (-) (mapUpperBounds state) nextGuardPos)
        then False
        else loops (state { guardPosition = nextGuardPos }, S.insert (guardPosition state, guardDirection state) visited)

part2 :: Indata -> Int
part2 indata = length $ filter (\newObstacle -> loops (indata {mapObstacles = S.insert newObstacle $ mapObstacles indata }, S.empty))
    [V2 x y | let V2 maxX maxY = mapUpperBounds indata, x <- [0..maxX], y <- [0..maxY]]
