import qualified Data.Map as M
import Utils
import Debug.Trace

main :: IO ()
main = do
    let indata = [2,0,6,12,1,3]
    print $ part1 indata
    print $ part2 indata

part1 :: [Int] -> Int
part1 indata = head $ drop (2020 - length indata) $ map snd $ speaking
    where
        constructed = M.fromList $ map (\(v, r) -> (v, (r, Nothing))) $ zip indata [0..]
        speaking = (constructed, last indata):map nextIteration speaking

part2 :: [Int] -> Int
part2 indata = head $ drop (30000000 - length indata) $ map snd $ speaking
    where
        constructed = M.fromList $ map (\(v, r) -> (v, (r, Nothing))) $ zip indata [0..]
        speaking = (constructed, last indata):map nextIteration speaking

nextIteration :: (M.Map Int (Int, Maybe Int), Int) -> (M.Map Int (Int, Maybe Int), Int)
nextIteration (spoken, lastSpoken) = (nextMap, next)
    where
        (next, lastIter) = case spoken M.! lastSpoken of
            (early, Just late) -> (late - early, late)
            (early, Nothing) -> (0, early)
        nextMap = M.alter mapUpdater next spoken
        mapUpdater Nothing = Just (succ lastIter, Nothing)
        mapUpdater (Just (prev, Nothing)) = Just (prev, Just $ succ lastIter)
        mapUpdater (Just (early, Just late)) = Just (late, Just $ succ lastIter)
