import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Control.Monad.State
import Utils

main :: IO ()
main = do
    indata <- readFile "10.in"
    print $ part1 indata
    print $ part2 indata

part1 :: String -> Int
part1 indata = let (low, high) = foldl countAdapters (0, 0) $ map (\(l:h:_) -> h - l) $ windows 2 $ ([0] ++ adapters ++ [maximum adapters + 3]) in low * high
    where
        adapters = sort $ map read $ lines indata

countAdapters :: (Int, Int) -> Int -> (Int,Int)
countAdapters (l, h) 1 = (succ l, h)
countAdapters (l, h) 3 = (l, succ h)
countAdapters (l, h) _ = (l, h)

part2 :: String -> Integer
part2 indata = fst $ flip runState (M.singleton 0 1) (endsWith adapters' $ maximum adapters') 
    where
        adapters = sort $ map read $ lines indata
        adapters' = S.fromList $ [0] ++ adapters ++ [maximum adapters + 3]

endsWith :: S.Set Integer -> Integer -> State (M.Map Integer Integer) Integer
endsWith adapters on = do
    if S.member on adapters
        then do
            mem <- get
            if M.member on mem
                then return $ mem M.! on
                else do
                    a <- endsWith adapters $ on - 1
                    b <- endsWith adapters $ on - 2
                    c <- endsWith adapters $ on - 3
                    let val = a + b + c
                    state (\mem -> (val, M.insert on val mem))
        else return 0
