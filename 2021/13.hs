import Utils
import qualified Data.Set as S
import Data.Ord
import Data.List

type Point = (Int, Int)
type Indata = (S.Set Point, [Point -> Point])

main :: IO ()
main = do
    indata <- parseData <$> readFile "13.in"
    print $ part1 indata
    putStrLn $ part2 indata

parseData :: String -> Indata
parseData indata = 
    let (coords:folds:_) = splitWhen (=="") $ lines indata
        parseCoord l = let (x:y:_) = splitWhen (==',') l in (read x, read y)
    in (S.fromList $ fmap parseCoord coords, fmap parseFold folds)

parseFold :: String -> Point -> Point
parseFold l = if dir == 'x'
                then (\p@(x,y) -> if x < read num then p else (2 * read num - x, y))
                else (\p@(x,y) -> if y < read num then p else (x, 2 * read num - y))
    where
        (dir:_:num) = drop 11 l

part1 :: Indata -> Int
part1 indata = S.size . S.map (head $ snd indata) $ fst indata

part2 :: Indata -> String
part2 indata =  let coords = S.map (foldl (flip (.)) id (snd indata)) $ fst indata
                    minX = fst $ minimumBy (comparing fst) coords
                    maxX = fst $ maximumBy (comparing fst) coords
                    minY = snd $ minimumBy (comparing snd) coords
                    maxY = snd $ maximumBy (comparing snd) coords
                in unlines [[if (x,y) `S.member` coords then '#' else ' ' | x <- [minX..maxX]] | y <- [minY..maxY]]
