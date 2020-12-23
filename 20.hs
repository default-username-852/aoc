import Utils
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Control.Monad
import Debug.Trace

data Direction = N | S | W | E deriving (Show, Eq, Ord, Enum)

flipDir :: Direction -> Direction
flipDir N = S
flipDir S = N
flipDir E = W
flipDir W = E

main :: IO ()
main = do
    indata <- readFile "20.in"
    let parsed = parseData indata
    let placements = createMapping parsed
    print $ part1 $ M.map fst placements
    print $ part2 placements parsed

part1 :: M.Map Point Int -> Int
part1 finalMap = snd (maximumBy (\((x1, y1), _) ((x2, y2), _) -> x1 `compare` x2 <> y1 `compare` y2) finalMap')
    * snd (maximumBy (\((x1, y1), _) ((x2, y2), _) -> x2 `compare` x1 <> y1 `compare` y2) finalMap')
    * snd (maximumBy (\((x1, y1), _) ((x2, y2), _) -> x1 `compare` x2 <> y2 `compare` y1) finalMap')
    * snd (maximumBy (\((x1, y1), _) ((x2, y2), _) -> x2 `compare` x1 <> y2 `compare` y1) finalMap')
    where
        finalMap' = M.assocs finalMap

monster :: [Point]
monster = [(0,0),(1,-1),(4,-1),(5,0),(6,0),(7,-1),(10,-1),(11,0),(12,0),(13,-1),(16,-1),(17,0),(18,0),(18,1),(19,0)]

allMonsters :: [[Point]]
allMonsters = do
    rotation <- take 4 $ iterate (matMat ((0,-1),(1,0)) .) id
    mirroring <- [id, matMat ((1,0),(0,-1))]
    return $ map (matVec (mirroring $ rotation ((1,0),(0,1)))) monster

part2 :: M.Map Point (Int, TileData -> TileData) -> [Tile] -> Int
part2 placements tiles = head $ do
    checking <- allMonsters
    let monsterCount = length $ filter (\pos -> all (\offset -> addVec pos offset `S.member` occupiedTiles) checking) $ S.elems occupiedTiles
    guard $ monsterCount > 0
    return $ S.size occupiedTiles - 15 * monsterCount
    where
        tiles' = M.fromList tiles
        placements' = M.assocs placements
        occupiedTiles = S.fromList $ map fst $ filter snd $ (\(pos, (tileId, transform)) -> blockTranslate pos (transform $ tiles' M.! tileId)) =<< placements'

blockTranslate :: Point -> TileData -> [(Point, Bool)]
blockTranslate (xBlock, yBlock) mat = (\(rowIdx, row) -> map (\(colIdx, bool) -> ((colIdx + 8 * xBlock, rowIdx + 8 * yBlock), bool)) $ zip [0..] $ init $ tail row) =<< zip [7,6..] (init $ tail mat)

createMapping :: [Tile] -> M.Map Point (Int, TileData -> TileData)
createMapping ((startTileId, startTileData):tiles) = finalMap
    where
        startState = (M.fromList $ map (\dir -> (offsetPoint (0, 0) dir, M.singleton (flipDir dir) (getNum startTileData dir))) [N .. E], M.singleton (0, 0) (startTileId, id), S.fromList tiles)
        Just (_, finalMap, _) = find (\(_, _, tilesLeft) -> null tilesLeft) $ head $ drop (length tiles) $ iterate (>>= addTile) $ return startState

parseData :: String -> [Tile]
parseData indata = do
    block <- splitWhen (=="") $ lines indata
    let (header:_, body) = splitAt 1 block
    let mat = map (map (=='#')) body
    let mat' = transpose mat
    return (read $ init $ (!! 1) $ words header, mat)

toInt :: [Bool] -> Int
toInt = foldl (\i b -> (if b then 1 else 0) + 2 * i) 0

-- (x, y)
type Point = (Int, Int)
type Mat2 = ((Int, Int), (Int, Int))

addVec :: Point -> Point -> Point
addVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

matVec :: Mat2 -> Point -> Point
matVec ((a, b), (c, d)) (x, y) = (a*x + b*y, c*x + d*y)

matMat :: Mat2 -> Mat2 -> Mat2
matMat ((a1, b1), (c1, d1)) ((a2, b2), (c2, d2)) = ((a1*a2+b1*c2, a1*b2+b1*d2), (c1*a2+d1*c2, c1*b2+d1*d2))

offsetPoint :: Point -> Direction -> Point
offsetPoint (x, y) N = (x, succ y)
offsetPoint (x, y) S = (x, pred y)
offsetPoint (x, y) E = (succ x, y)
offsetPoint (x, y) W = (pred x, y)

-- (id, data)
type Tile = (Int, TileData)

type TileData = [[Bool]]

addTile :: (M.Map Point (M.Map Direction Int), M.Map Point (Int, TileData -> TileData), S.Set Tile) -> [(M.Map Point (M.Map Direction Int), M.Map Point (Int, TileData -> TileData), S.Set Tile)]
addTile (points, placed, tiles) = do
    (pos, directions) <- M.assocs points
    let constraints = M.assocs directions
    (tileID, tileData) <- S.elems tiles
    let rest = S.delete (tileID, tileData) tiles
    (tile', transform) <- everyOrientation tileData
    guard $ matchesConstraints tile' constraints
    let newDirs = [N .. E] \\ map fst constraints
    let newMap = foldl (\m dir -> insertDefault m (offsetPoint pos dir) M.empty (\m' -> M.insert (flipDir dir) (getNum tile' dir) m')) (M.delete pos points) newDirs
    return (newMap, M.insert pos (tileID, transform) placed, rest)

insertDefault :: (Ord k) => M.Map k v -> k -> v -> (v -> v) -> M.Map k v
insertDefault m k v func = M.alter updateFunc k m
    where
        updateFunc Nothing = Just $ func v
        updateFunc (Just v') = Just $ func v'

getNum :: TileData -> Direction -> Int
getNum mat N = toInt $ head mat
getNum mat S = toInt $ last mat
getNum mat E = toInt $ last $ transpose mat
getNum mat W = toInt $ head $ transpose mat

matchesConstraints :: TileData -> [(Direction, Int)] -> Bool
matchesConstraints tile cs = all (matchesConstraint tile) cs

matchesConstraint :: TileData -> (Direction, Int) -> Bool
matchesConstraint mat (dir, num) = num == getNum mat dir

everyOrientation :: TileData -> [(TileData, TileData -> TileData)]
everyOrientation tile = do
    rot <- take 4 $ iterate (. rotateTile) id
    mirror <- [id, mirrorTile]
    return  (rot $ mirror tile, rot . mirror)

rotateTile :: TileData -> TileData
rotateTile = transpose . reverse

mirrorTile :: TileData -> TileData
mirrorTile = transpose
