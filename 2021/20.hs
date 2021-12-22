import Utils
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Function
import Data.Ord

type Point = (Int, Int)
type Indata = (S.Set Int, M.Map Point Bool)

main :: IO ()
main = do
    indata <- parseData <$> readFile "20.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData indata = 
    let (enhance:_:rest) = lines indata 
        pts = M.fromList $ (\(r, l) -> fmap (\(c,e) -> ((c,r),e=='#')) $ zip [0..] l) =<< zip [0..] rest
    in (S.fromList . fmap fst . filter ((=='#') . snd) $ zip [0..] enhance, pts)

covering :: Point -> [Point]
covering (x,y) = [ (x-1,y-1)
                 , (x,y-1)
                 , (x+1,y-1)
                 , (x-1,y)
                 , (x,y)
                 , (x+1,y)
                 , (x-1,y+1)
                 , (x,y+1)
                 , (x+1,y+1)
                 ]

getIdx :: Point -> Int -> M.Map Point Bool -> Int
getIdx p iter img = foldl (\acc e -> if M.findWithDefault (iter `mod` 2 == 1) e img then 2 * acc + 1 else 2 * acc) 0 $ covering p

enhanceImage :: S.Set Int -> Int -> M.Map Point Bool -> M.Map Point Bool
enhanceImage enhance iter img = M.fromList . fmap (\p -> (p, getIdx p iter img `S.member` enhance)) $ covering =<< M.keys img

part1 :: Indata -> Int
part1 (enhance, img) = img & enhanceImage enhance 0 & enhanceImage enhance 1 & M.filter id & M.size

part2 :: Indata -> Int
part2 (enhance, img) = fmap (enhanceImage enhance) [0..49] & foldl (&) img & M.filter id & M.size
