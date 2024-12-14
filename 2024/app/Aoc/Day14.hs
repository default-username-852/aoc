{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Aoc.Day14(solve) where
import Linear ( V2(..) )
import Text.Regex.TDFA
import Control.Applicative (Applicative (liftA2))
import Data.Array (array, Ix (range), (//), elems)
import Aoc.Utils (chunksOf)
import Text.Printf (printf)
import qualified Data.Set as S
import Data.List (transpose)

type Indata = [Robot]

data Robot = Robot (V2 Int) (V2 Int) deriving Show

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    part2 parsed

parseRobot :: String -> Robot
parseRobot r =
    let (_,_,_,px:py:vx:vy:_) = r =~ "p=([0-9]+),([0-9]+) v=(-?[0-9]+),(-?[0-9]+)" :: (String, String, String, [String])
    in Robot (V2 (read px) (read py)) (V2 (read vx) (read vy))

parseData :: String -> Indata
parseData = fmap parseRobot . lines

mapSize :: V2 Int
mapSize = V2 101 103

stepRobot :: Robot -> Robot
stepRobot (Robot p v) = Robot (liftA2 (flip mod) mapSize $ p + v + mapSize) v

countQuadrants :: [Robot] -> (Int, Int, Int, Int)
countQuadrants robots =
    let (V2 mapWidth mapHeight) = mapSize
        midHorizontal = mapWidth `div` 2
        midVertical = mapHeight `div` 2
        topLeft (Robot (V2 x y) _) = x < midHorizontal && y < midVertical
        topRight (Robot (V2 x y) _) = x > midHorizontal && y < midVertical
        bottomLeft (Robot (V2 x y) _) = x < midHorizontal && y > midVertical
        bottomRight (Robot (V2 x y) _) = x > midHorizontal && y > midVertical
    in (length $ filter topLeft robots, length $ filter topRight robots, length $ filter bottomLeft robots, length $ filter bottomRight robots)

part1 :: Indata -> Int
part1 indata = let (a,b,c,d) = countQuadrants $ iterate (fmap stepRobot) indata !! 100 in a*b*c*d

clustering :: [Robot] -> Int
clustering robots =
    let asSet = S.fromList $ fmap (\(Robot p _) -> p) robots
        adjacencies = [V2 dx dy | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]
        countNeighbours (Robot p _) = length . filter (`S.member` asSet) $ fmap (p+) adjacencies
    in sum $ fmap countNeighbours robots

printMap :: [Robot] -> Int -> IO ()
printMap robots step = do
    let world = array (V2 0 0, mapSize - 1) [(i, ' ') | i <- range (V2 0 0, fmap (+ (-1)) mapSize)] // [(p, '#') | (Robot p _) <- robots]
    let (V2 _ mapHeight) = mapSize
    let stringified = unlines . transpose $ chunksOf mapHeight $ elems world
    writeFile ("14/" ++ printf "%05d" step) stringified

part2 :: Indata -> IO ()
part2 indata = (mapM_ (uncurry $ flip printMap) . filter ((>=250) . clustering . snd) . zip [0..] . take (101*103)) $ iterate (fmap stepRobot) indata
