import qualified Data.Set as S
import qualified Data.Map as M
import Utils
import Data.List
import Control.Monad

main :: IO ()
main = do
    indata <- readFile "16.in"
    print $ part1 indata
    print $ part2 indata

part1 :: String -> Int
part1 indata = let (_, nums, valid) = parseData indata in sum $ filter (not . flip S.member (S.unions valid)) $ concat nums

part2 :: String -> Int
part2 indata = foldl (*) 1 $ map (\(_, c) -> mine !! c) $ filter ((<6) . fst) $ head $ associateCols $ sortOn (length . snd) $ M.assocs $ foldl mapAcc M.empty $ do
    (fieldIdx, field) <- zip [0..] fields
    (colIdx, trying) <- zip [0..] $ transpose others'
    guard $ all (flip S.member field) trying
    return (fieldIdx, colIdx)
    where
        (mine, others, fields) = parseData indata
        others' = filter (all $ flip S.member (S.unions fields)) others
        mapAcc m (f, c) = M.alter (listAdder c) f m
        listAdder a Nothing = Just [a]
        listAdder a (Just l) = Just $ a:l

parseData :: String -> ([Int], [[Int]], [S.Set Int])
parseData indata = (your, nums, valids)
    where
        (fields:mine:others:_) = splitWhen (=="") $ lines indata
        valids = do
            field <- fields
            let (_:f1:_:f2:_) = words field
            let (low1:high1:_) = splitWhen (=='-') f1
            let (low2:high2:_) = splitWhen (=='-') f2
            return $ S.fromList [read low1..read high1] `S.union` S.fromList [read low2..read high2]
        mine' = head $ tail mine
        your = map read $ splitWhen (==',') mine'
        nums = map (map read . splitWhen (==',')) $ tail others

associateCols :: [(Int, [Int])] -> [[(Int, Int)]]
associateCols assocs
    | null assocs = return []
    | otherwise = do
        let (field, cols) = head assocs
        let map' = tail assocs
        col <- cols
        let map'' = map (\(a, b) -> (a, delete col b)) map'
        alternative <- associateCols map''
        return $ (field, col):alternative
