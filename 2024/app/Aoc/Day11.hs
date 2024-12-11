{-# LANGUAGE TupleSections #-}
module Aoc.Day11(solve) where
import qualified Data.Map.Strict as M

type Indata = [Int]

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

parseData :: String -> Indata
parseData = fmap read . words

updateStone :: Int -> [Int]
updateStone 0 = [1]
updateStone stone
    | even . length $ show stone =
        let strRep = show stone
            l = length strRep
            (a, b) = splitAt (l `div` 2) strRep
        in [read a, read b]
    | otherwise = [2024*stone]

stepStones :: [Int] -> [Int]
stepStones = (>>= updateStone)

part1 :: Indata -> Int
part1 indata = length $ iterate stepStones indata !! 25

stepStones2 :: M.Map Int Int -> M.Map Int Int
stepStones2 stones =
    foldr (M.unionWith (+)) M.empty . concatMap (\(stoneVal, count) -> (`M.singleton` count) <$> updateStone stoneVal) $ M.assocs stones


part2 :: Indata -> Int
part2 indata =
    let asMap = M.fromList $ fmap (,1) indata
    in sum $ iterate stepStones2 asMap !! 75
