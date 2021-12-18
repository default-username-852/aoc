import Utils
import Graph
import qualified Data.Map as M
import Data.Maybe

type Point = (Int, Int)
type Indata = WeightedGraph Point

main :: IO ()
main = do
    indata <- readFile "15.in"
    print . part1 $ parseData indata
    print . part2 . parseData $ enlargeMap indata

parseData :: String -> Indata
parseData indata = M.mapWithKey (\node _ -> catMaybes . fmap (\n -> fmap ((,) n) $ M.lookup n coords) $ adjacent node) coords
    where
        coords :: M.Map Point Int
        coords = M.fromList $ (\(r, l) -> fmap (\(c, num) -> ((r,c), read [num])) $ zip [0..] l) =<< zip [0..] (lines indata)

bumpNum :: Char -> Char
bumpNum '9' = '1'
bumpNum a = succ a

enlargeMap :: String -> String
enlargeMap indata = unlines . concat . take 5 . iterate (fmap fmap fmap bumpNum) . fmap (concat . take 5 . iterate (fmap bumpNum)) $ lines indata

adjacent :: Point -> [Point]
adjacent (x,y) = [ (x+1,y)
                 , (x-1,y)
                 , (x,y+1)
                 , (x,y-1)
                 ]

part1 :: Indata -> Int
part1 indata = snd . fromJust $ dijkstra indata (0,0) (99, 99)

part2 :: Indata -> Int
part2 indata = snd . fromJust $ dijkstra indata (0,0) (499, 499)
