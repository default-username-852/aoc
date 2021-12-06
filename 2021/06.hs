import Utils
import qualified Data.Map as M

type Indata = [Int]

main :: IO ()
main = do
    indata <- parseData <$> readFile "06.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData = fmap read . splitWhen (==',')

advanceFish :: Int -> [Int]
advanceFish 0 = [6,8]
advanceFish a = return $ a - 1

advanceFish' :: M.Map Int Int -> M.Map Int Int
advanceFish' m = M.singleton 6 (M.findWithDefault 0 0 m) `inserter` M.singleton 8 (M.findWithDefault 0 0 m) `inserter` (M.mapKeys (\x -> x - 1) $ M.delete 0 m)
    where
        inserter :: M.Map Int Int -> M.Map Int Int -> M.Map Int Int
        inserter = M.unionWith (+)

part1 :: Indata -> Int
part1 indata = length $ iterate (>>=advanceFish) indata !! 80

part2 :: Indata -> Int
part2 indata = sum . (!! 256) . iterate advanceFish' $ foldl (\m v -> M.insertWith (+) v 1 m) M.empty indata
