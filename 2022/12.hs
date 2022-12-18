import Utils
import Graph
import Data.Char
import Data.Maybe

type Indata = [[Char]]
type Vec2 = (Int, Int)

main :: IO ()
main = do
    indata <- parseData <$> readFile "12.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData = lines

part1 :: Indata -> Int
part1 indata = 
    let Just (_, steps) = dijkstra (neighbour indata) (0,20) (43,20)
    in steps

neighbour :: [[Char]] -> Vec2 -> [(Vec2, Int)]
neighbour indata (x,y) = 
    let at = (indata !! y) !! x
        candidates = filter (\(px,py) -> px >= 0 && px < width && py >= 0 && py < height)
            [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
        candidates' = filter (\(cx,cy) -> ord at - ord (flip const (show (cx,cy,((indata !! cy) !! cx))) ((indata !! cy) !! cx)) >= (-1)) candidates
    in fmap (\p -> (p, 1)) candidates'
    where
        height = length indata
        width = length $ indata !! 0

part2 :: Indata -> Int
part2 indata = minimum $ fmap (\s -> fromMaybe 1000000 . fmap snd $ dijkstra (neighbour indata) s (43,20)) starts
    where
        starts = [(x,y) | y <- [0..length indata - 1], x <- [0..length (indata !! 0) - 1], (indata !! y) !! x == 'a']
