import Utils
import Control.Monad
import Prelude hiding (minBound, maxBound)

type Point = (Int, Int)

minBound :: Point
minBound = (185,-74)

maxBound :: Point
maxBound = (221,-122)

main :: IO ()
main = do
    print $ part1 minBound maxBound
    print $ part2 minBound maxBound

inside :: Point -> Point -> Point -> Bool
inside (minX,minY) (maxX,maxY) (x,y) = x <= maxX && x >= minX && y >= maxY && y <= minY

before :: Point -> Point -> Point -> Bool
before l@(minX,minY) r@(maxX,maxY) p@(x,y) = not (inside l r p) && x <= maxX && y <= maxY

after :: Point -> Point -> Point -> Bool
after (minX,minY) (maxX,maxY) (x,y) = x > maxX || y < maxY

step :: (Point, Point) -> (Point, Point)
step ((vx,vy),(px,py)) = ((vx-signum vx, vy-1),(px+vx,py+vy))

hits :: Point -> Point -> Point -> Bool
hits min max v = any (inside min max) . takeWhile (not . after min max) . fmap snd $ iterate step (v,(0,0))

part1 :: Point -> Point -> Int
part1 min@(minX,minY) max@(maxX, maxY) = maximum $ do
    vx <- [0..maxX]
    vy <- [maxY.. -maxY]
    guard $ hits min max (vx,vy)
    return $ if vy < 0 then 0 else vy * (vy + 1) `div` 2

part2 :: Point -> Point -> Int
part2 min@(minX,minY) max@(maxX, maxY) = length $ do
    vx <- [0..maxX]
    vy <- [maxY.. -maxY]
    guard $ hits min max (vx,vy)
    return ()
