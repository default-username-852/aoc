import qualified Data.Map as M
import qualified Data.List as L
import Utils
import Graph

main :: IO ()
main = do
    indata <- readFile "7.in"
    print $ part1 indata - 1
    print $ part2 indata - 1

part1 :: String -> Int
part1 indata = length $ filter id $ map (flip (bfs $ M.map (map snd) graph) "shiny gold") $ M.keys graph
    where
        graph = parseData indata

parseData :: String -> M.Map String [(Int, String)]
parseData indata = M.fromList $ do
    node <- lines indata
    let (col, rest) = splitAt 2 $ words node
    let rest' = drop 2 rest
    let to = if take 2 rest' == ["no", "other"] then [] else map (\(num:col1:col2:_) -> (read num, col1 ++ " " ++ col2)) $ chunksOf 4 rest'
    return (L.intercalate " " col, to)

part2 :: String -> Int
part2 indata = bagsIn (parseData indata) "shiny gold"

bagsIn :: M.Map String [(Int, String)] -> String -> Int
bagsIn graph = succ . sum . map (\(count, kind) -> count * bagsIn graph kind) . (M.!) graph

