import Utils
import Data.Vec.Base (setElem, vec, Mat33, Vec3, (:.)(..))
import qualified Data.Vec.Base as V (foldl)
import Data.Vec.LinAlg (det, multmv, multmm, identity)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Ord
import Control.Monad
import Debug.Trace

type Indata = [[Vec3 Int]]

main :: IO ()
main = do
    indata <- parseData <$> readFile "19.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData indata = 
    let groups = splitWhen (=="") $ lines indata
    in fmap parseGroup groups

parseGroup :: [String] -> [Vec3 Int]
parseGroup (_:g) = fmap parseVec g

parseVec :: String -> Vec3 Int
parseVec l = let (a:b:c:_) = splitWhen (==',') l in read a:.read b:.read c:.()

allRots :: [Mat33 Int]
allRots = do
    r1 <- [-1,1]
    r2 <- [-1,1]
    r3 <- [-1,1]
    row1 <- [0..2]
    row2 <- filter (/=row1) [0..2]
    row3 <- filter (\r -> r/=row1 && r /= row2) [0..2]
    let row1' = setElem row1 r1 $ vec 0
    let row2' = setElem row2 r2 $ vec 0
    let row3' = setElem row3 r3 $ vec 0
    let mat = row1':.row2':.row3':.()
    guard $ det mat == 1
    return mat

maxCommonDiffs :: [Vec3 Int] -> [Vec3 Int] -> (Vec3 Int, Int)
maxCommonDiffs as bs = maximumBy (comparing snd) . fmap (\ds -> (head ds, length ds)) . group $ sort [a-b | a <- as, b <- bs]
--maxCommonDiffs as bs = maximumBy (comparing snd) $ do
--    b <- bs
--    let d = head as - b
--    let moved = fmap (+d) bs
--    let overlap = S.fromList moved `S.intersection` S.fromList as
--    --if S.size overlap >= 12 then traceShowM overlap else pure ()
--    return (d, S.size overlap)

overlaps :: [Vec3 Int] -> [Vec3 Int] -> Maybe (Mat33 Int, Vec3 Int)
overlaps pointsA pointsB = listToMaybe $ do
    rot <- allRots
    let rotated = fmap (multmv rot) pointsB
    let (off, cnt) = maxCommonDiffs pointsA rotated
    guard $ cnt >= 12
    return (rot, off)

placeScanner :: [([Vec3 Int], (Mat33 Int, Vec3 Int))] -> [Vec3 Int] -> Maybe (Mat33 Int, Vec3 Int)
placeScanner placed placing = listToMaybe . catMaybes $ fmap (\(ps, (r,o)) -> fmap (\(r',o') -> (r', o + o')) $ overlaps (fmap (multmv r) ps) placing) placed

tryPlaceScanner :: ([([Vec3 Int], (Mat33 Int, Vec3 Int))], [[Vec3 Int]]) -> ([([Vec3 Int], (Mat33 Int, Vec3 Int))], [[Vec3 Int]])
tryPlaceScanner (placed, placing) = let (a,x:xs) = break (isJust . placeScanner placed) placing in ((x, fromJust (placeScanner placed x)):placed, a++xs)

genPoints :: Indata -> [Vec3 Int]
genPoints indata = S.toList . S.fromList . concat . fmap (\(ps, (rot, off)) -> fmap (transformPoint off rot) ps) . fst . head . dropWhile (not . null . snd) $ iterate tryPlaceScanner ([(head indata, (identity, 0:.0:.0:.()))], tail indata)
    where
        transformPoint :: Vec3 Int -> Mat33 Int -> Vec3 Int -> Vec3 Int
        transformPoint offset rot p = multmv rot p + offset

part1 :: Indata -> Int
part1 indata = length $ genPoints indata

part2 :: Indata -> Int
part2 indata = maximum $ do
    let ps = fmap (\(ps, (rot, off)) -> off) . fst . head . dropWhile (not . null . snd) $ iterate tryPlaceScanner ([(head indata, (identity, 0:.0:.0:.()))], tail indata)
    p1 <- ps
    p2 <- ps
    let res = V.foldl (\d v -> d + abs v) 0 (p1-p2)
    return $ res
