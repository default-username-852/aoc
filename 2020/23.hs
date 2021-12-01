import Utils
import Data.List
import Debug.Trace
import qualified Data.Map as M
import Data.Map ((!))
import Data.Maybe

indata :: [Int]
indata = map (read . (:[])) "476138259"

main :: IO ()
main = print (part1 indata) >> print (part2 indata)

part1 :: [Int] -> M.Map Int Int
part1 indata = snd $ (!! 100) $ iterate (doMove $ maximum indata) (head indata, M.insert (last indata) (head indata) $ foldl (\m (from:to:_) -> M.insert from to m) M.empty $ windows 2 indata)

part2 :: [Int] -> Int
part2 indata = (lst ! 1) * (lst ! (lst ! 1))
    where
        indata' = take 1000000 $ indata ++ [length indata + 1..]
        (_, lst) = (!! 1000000) $ iterate (doMove $ maximum indata') (head indata', M.insert (last indata') (head indata') $ foldl (\m (from:to:_) -> M.insert from to m) M.empty $ windows 2 indata')

doMove :: Int -> (Int, M.Map Int Int) -> (Int, M.Map Int Int)
doMove maxn (current, lst) = (newCurrent, lst')
    where
        p1 = lst ! current
        p2 = lst ! p1
        p3 = lst ! p2
        restHead = lst ! p3
        Just finding = find (not . flip elem [p1, p2, p3]) $ [current - 1, current - 2 .. 1] ++ [maxn, maxn - 1 .. current]
        newTail = lst ! finding
        lst' = M.insert p3 newTail $ M.insert finding p1 $ M.insert current restHead lst
        newCurrent = lst' ! current
