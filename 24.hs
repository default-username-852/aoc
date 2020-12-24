import Utils
import Data.List
import qualified Data.Set as S

main :: IO ()
main = do
    indata <- readFile "24.in"
    print $ part1 indata
    print $ part2 indata

part1 :: String -> Int
part1 indata = length $ filter odd $ map length $ group $ sort $ map ($ (0, 0, 0)) $ parseData indata

part2 :: String -> Int
part2 indata = S.size $ (!! 100) $ iterate generation $ S.fromList $ map head $ filter (odd . length) $ group $ sort $ map ($ (0, 0, 0)) $ parseData indata

parseData :: String -> [Vec3 -> Vec3]
parseData = map parseRow . lines

parseRow :: String -> (Vec3 -> Vec3)
parseRow [] v = v
parseRow ('s':'e':rest) (x, y, z) = parseRow rest (x, y-1, z+1)
parseRow ('s':'w':rest) (x, y, z) = parseRow rest (x-1, y, z+1)
parseRow ('n':'e':rest) (x, y, z) = parseRow rest (x+1, y, z-1)
parseRow ('n':'w':rest) (x, y, z) = parseRow rest (x, y+1, z-1)
parseRow ('e':rest) (x, y, z) = parseRow rest (x+1, y-1, z)
parseRow ('w':rest) (x, y, z) = parseRow rest (x-1, y+1, z)

type Vec3 = (Int, Int, Int)

generation :: S.Set Vec3 -> S.Set Vec3
generation alive = S.union (S.filter (nextState alive) alive) (S.fromList $ filter (nextState alive) $ filter (not . flip S.member alive) $ neighbours =<< S.elems alive)

nextState :: S.Set Vec3 -> Vec3 -> Bool
nextState tiles on
    | S.member on tiles = aliveNeighbours == 1 || aliveNeighbours == 2
    | otherwise = aliveNeighbours == 2
    where 
        aliveNeighbours = length $ filter (flip S.member tiles) $ neighbours on

neighbours :: Vec3 -> [Vec3]
neighbours (x, y, z) = [(x+1,y-1,z),(x-1,y+1,z),(x+1,y,z-1),(x-1,y,z+1),(x,y+1,z-1),(x,y-1,z+1)]

aliveNeighbours :: S.Set Vec3 -> Vec3 -> Int
aliveNeighbours tiles = length . filter (flip S.member tiles) . neighbours
