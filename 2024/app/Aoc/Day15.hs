module Aoc.Day15(solve) where
import Data.Array (Array, array, (!), (//), assocs, bounds)
import Linear
import Data.Maybe (fromJust)
import Data.List (find)
import Prelude hiding (map)
import Control.Monad

type Indata = (Array (V2 Int) MapTile, V2 Int, [V2 Int])

data MapTile = Wall | Empty | Box | Robot deriving Eq

instance Show MapTile where
  show Wall = "#"
  show Empty = "."
  show Box = "O"
  show Robot = "@"


solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

toTile :: Char -> MapTile
toTile '#' = Wall
toTile '.' = Empty
toTile 'O' = Box
toTile '@' = Robot
toTile c = error $ "unknown char : \"" <> [c] <> "\""

moveToDir :: Char -> V2 Int
moveToDir '^' = V2 0 (-1)
moveToDir '<' = V2 (-1) 0
moveToDir 'v' = V2 0 1
moveToDir '>' = V2 1 0
moveToDir c = error $ "unknown direction: " <> [c]

parseData :: String -> Indata
parseData input =
    let (map, moves) = break (=="") $ lines input
        annotatedMap = concatMap (\(y, row) -> (\(x, c) -> (V2 x y, toTile c)) <$> zip [0..] row) $ zip [0..] map
    in (array (V2 0 0, maximum $ fmap fst annotatedMap) annotatedMap, fst . fromJust $ find ((==Robot) . snd) annotatedMap, concatMap (fmap moveToDir) moves)

makeMove :: V2 Int -> V2 Int -> Array (V2 Int) MapTile -> Maybe (Array (V2 Int) MapTile)
makeMove movingTile moveDir map =
    let newPos = movingTile + moveDir
        mapUpdate = [(newPos, map ! movingTile), (movingTile, Empty)]
    in case map ! newPos of
        Wall -> Nothing
        Empty -> Just $ map // mapUpdate
        _ -> (// mapUpdate) <$> makeMove newPos moveDir map

makeMoves :: [V2 Int] -> V2 Int -> Array (V2 Int) MapTile -> Array (V2 Int) MapTile
makeMoves [] _ map = map
makeMoves (move:moves) robotPos map = case makeMove robotPos move map of
    Nothing -> makeMoves moves robotPos map
    Just newMap -> makeMoves moves (robotPos + move) newMap

scoreTile :: V2 Int -> MapTile -> Int
scoreTile (V2 x y) Box = 100 * y + x
scoreTile _ _ = 0

part1 :: Indata -> Int
part1 (map, robotStartPos, moves) = sum . fmap (uncurry scoreTile) . assocs $ makeMoves moves robotStartPos map

data BigMapTile = BWall | BEmpty | BRobot | BLeftBox | BRightBox deriving (Eq, Show)

makeMapBig :: Array (V2 Int) MapTile -> Array (V2 Int) BigMapTile
makeMapBig map = array ((\(V2 x y) -> V2 (2*x + 1) y) <$> bounds map) . concatMap expandTile $ assocs map
    where
        expandTile :: (V2 Int, MapTile) -> [(V2 Int, BigMapTile)]
        expandTile (V2 x y, Wall) = [(V2 (2*x) y, BWall), (V2 (2*x + 1) y, BWall)]
        expandTile (V2 x y, Box) = [(V2 (2*x) y, BLeftBox), (V2 (2*x + 1) y, BRightBox)]
        expandTile (V2 x y, Empty) = [(V2 (2*x) y, BEmpty), (V2 (2*x + 1) y, BEmpty)]
        expandTile (V2 x y, Robot) = [(V2 (2*x) y, BRobot), (V2 (2*x + 1) y, BEmpty)]

makeBigMove :: V2 Int -> V2 Int -> Array (V2 Int) BigMapTile -> Maybe (Array (V2 Int) BigMapTile)
makeBigMove movingTile moveDir map =
    let newPos = movingTile + moveDir
        mapUpdate = [(newPos, map ! movingTile), (movingTile, BEmpty)]
    in case map ! newPos of
        BWall -> Nothing
        BEmpty -> Just $ map // mapUpdate
        BLeftBox -> do
            map2 <- makeBigMove newPos moveDir map
            map3 <- if (\(V2 x _) -> x) moveDir == 0 then
                makeBigMove (newPos + V2 1 0) moveDir map2
                else pure map2
            pure $ map3 // mapUpdate
        BRightBox -> do
            map2 <- makeBigMove newPos moveDir map
            map3 <- if (\(V2 x _) -> x) moveDir == 0 then
                makeBigMove (newPos + V2 (-1) 0) moveDir map2
                else pure map2
            pure $ map3 // mapUpdate
        BRobot -> error "tried to push robot"
        --BRobot -> (// mapUpdate) <$> makeBigMove newPos moveDir map

makeBigMoves :: [V2 Int] -> V2 Int -> Array (V2 Int) BigMapTile -> Array (V2 Int) BigMapTile
makeBigMoves [] _ map = map
makeBigMoves (move:moves) robotPos map = case makeBigMove robotPos move map of
    Nothing -> makeBigMoves moves robotPos map
    Just newMap -> makeBigMoves moves (robotPos + move) newMap

scoreBigTile :: V2 Int -> BigMapTile -> Int
scoreBigTile (V2 x y) BLeftBox = 100 * y + x
scoreBigTile _ _ = 0

part2 :: Indata -> Int
part2 (map, robotStartPos, moves) = sum . fmap (uncurry scoreBigTile) . assocs $ makeBigMoves moves ((\(V2 x y) -> V2 (2*x) y) robotStartPos) (makeMapBig map)

