import Utils
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.List
import Data.Ord
import Debug.Trace

type Point = (Int, Int)
type Indata = M.Map Point Int

main :: IO ()
main = do
    indata <- parseData <$> readFile "09.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData indata = M.fromList $ (\(r, l) -> fmap (\(c, e) -> ((r,c),read $ e:[])) $ zip [0..] l) =<< zip [0..] (lines indata)

adjacent :: Point -> [Point]
adjacent (x,y) = [ (x+1,y)
                 , (x-1,y)
                 , (x,y+1)
                 , (x,y-1)
                 ]

lowPoint :: Point -> M.Map Point Int -> Bool
lowPoint p m = let h = m M.! p in all ((>h) . (flip (M.findWithDefault 10) m)) $ adjacent p

floodFill :: Point -> M.Map Point Int -> S.Set Point
floodFill p m = fst . last . takeWhile (not . null . snd) $ iterate (\(v,ps) -> let (v', ps') = step v $ head ps in (v', tail ps ++ ps')) (S.singleton p, return p)
    where
        level = m M.! p
        step :: S.Set Point -> Point -> (S.Set Point, [Point])
        step visited on = let newN = filter (not . flip S.member visited) . filter ((<=level) . (flip (M.findWithDefault 10) m)) $ adjacent on in (S.fromList newN `S.union` visited, newN)

basinBottom :: Point -> M.Map Point Int -> Point
basinBottom p m = minimumBy (comparing (m M.!)) . S.elems $ floodFill p m

part1 :: Indata -> Int
part1 indata = sum . fmap ((+1) . (indata M.!)) . filter (flip lowPoint indata) $ M.keys indata

part2 :: Indata -> Int
part2 indata = let (a:b:c:_) = reverse . sort . M.elems . foldl (flip (flip (M.insertWith (+)) 1)) M.empty . fmap (flip basinBottom indata) . filter ((/=9) . (indata M.!)) $ M.keys indata in a * b * c
