import qualified Data.Map as M
import qualified Data.Set as S
import Utils
import Debug.Trace

main :: IO ()
main = do
    indata <- readFile "17.in"
    print $ part1 indata
    print $ part2 indata

part1 :: String -> Int
part1 indata = S.size $ flip (!!) 6 $ iterate nextState $ parseData indata

parseData :: String -> S.Set Vec3
parseData indata = S.fromList $ concat $ map (\(y, row) -> map (\(x, _) -> Vec3 x y 0) row) $ map (\(y, row) -> (y, filter ((=='#') . snd) row)) $ zip [0..] $ map (zip [0..]) $ lines indata

data Vec3 = Vec3 Int Int Int deriving (Show, Ord, Eq)

nextState :: S.Set Vec3 -> S.Set Vec3
nextState current = S.fromList $ filter (flip aliveNext current) $ neighboursInclusive =<< S.toList current

aliveNext :: Vec3 -> S.Set Vec3 -> Bool
aliveNext pos alive
    | pos `S.member` alive = aliveNeighbours == 2 || aliveNeighbours == 3
    | otherwise = aliveNeighbours == 3
    where
        aliveNeighbours = length $ filter (flip S.member alive) $ neighbours pos

neighbours :: Vec3 -> [Vec3]
neighbours pos = filter (/= pos) $ neighboursInclusive pos

neighboursInclusive :: Vec3 -> [Vec3]
neighboursInclusive (Vec3 x y z) = do
    dx <- [-1..1]
    dy <- [-1..1]
    dz <- [-1..1]
    return $ Vec3 (x + dx) (y + dy) (z + dz)

part2 :: String -> Int
part2 indata = S.size $ flip (!!) 6 $ iterate nextState' $ parseData' indata

parseData' :: String -> S.Set Vec4
parseData' indata = S.fromList $ concat $ map (\(y, row) -> map (\(x, _) -> Vec4 x y 0 0) row) $ map (\(y, row) -> (y, filter ((=='#') . snd) row)) $ zip [0..] $ map (zip [0..]) $ lines indata

data Vec4 = Vec4 Int Int Int Int deriving (Show, Ord, Eq)

nextState' :: S.Set Vec4 -> S.Set Vec4
nextState' current = S.fromList $ filter (flip aliveNext' current) $ neighboursInclusive' =<< S.toList current

aliveNext' :: Vec4 -> S.Set Vec4 -> Bool
aliveNext' pos alive
    | pos `S.member` alive = aliveNeighbours == 2 || aliveNeighbours == 3
    | otherwise = aliveNeighbours == 3
    where
        aliveNeighbours = length $ filter (flip S.member alive) $ neighbours' pos

neighbours' :: Vec4 -> [Vec4]
neighbours' (Vec4 x y z w) = 
    [Vec4 x y z (w + 1), Vec4 x y z (w - 1)]
    ++ do {dw <- [-1..1]; [Vec4 x y (z + 1) (w + dw), Vec4 x y (z - 1) (w + dw)] }
    ++ do {dz <- [-1..1]; dw <- [-1..1]; [Vec4 x (y + 1) (z + dz) (w + dw), Vec4 x (y - 1) (z + dz) (w + dw)] }
    ++ do { dy <- [-1..1]; dz <- [-1..1]; dw <- [-1..1]; [Vec4 (x + 1) (y + dy) (z + dz) (w + dw), Vec4 (x - 1) (y + dy) (z + dz) (w + dw)] }

neighboursInclusive' :: Vec4 -> [Vec4]
neighboursInclusive' pos = pos:neighbours' pos
