import Utils
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Control.Monad.State.Lazy

type Point = (Int, Int)
type Indata = M.Map Point Int

main :: IO ()
main = do
    indata <- parseData <$> readFile "11.in"
    print $ part1 indata
    print $ part2 indata

adjacent :: Point -> [Point]
adjacent (x,y) = [ (x+1,y)
                 , (x-1,y)
                 , (x,y+1)
                 , (x,y-1)
                 , (x+1,y+1)
                 , (x-1,y+1)
                 , (x+1,y-1)
                 , (x-1,y-1)
                 ]

parseData :: String -> Indata
parseData indata = M.fromList $ (\(r, l) -> fmap (\(c, e) -> ((r,c), read $ e:[])) $ zip [0..] l) =<< zip [0..] (lines indata)

increaseEnergy :: M.Map Point Int -> M.Map Point Int
increaseEnergy = fmap (+1)

doFlash :: (S.Set Point, M.Map Point Int) -> Maybe (S.Set Point, M.Map Point Int)
doFlash (flashed, state) 
    | null flashing = Nothing
    | otherwise = Just (flashed `S.union` S.fromList flashing, foldl (\m p -> M.adjust (+1) p m) state $ flashing >>= adjacent)
    where
        flashing = filter (flip S.notMember flashed) . M.keys $ M.filter (>9) state

resetFlashed :: [Point] -> M.Map Point Int -> M.Map Point Int
resetFlashed ps m = foldl (flip $ M.adjust (const 0)) m ps

step :: M.Map Point Int -> (Int, M.Map Point Int)
step m = let (flashed, m') = fromJust . last . takeWhile isJust . iterate (>>=doFlash) $ Just (S.empty, increaseEnergy m) in (S.size flashed, resetFlashed (S.elems flashed) m')

part1 :: Indata -> Int
part1 indata = sum . flip evalState indata . sequence . replicate 100 $ state step

part2 :: Indata -> Int
part2 indata = (+1) . length . takeWhile (/=100) . flip evalState indata . sequence . repeat $ state step
