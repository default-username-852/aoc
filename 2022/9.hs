import Utils
import Control.Monad
import Data.List

data Movement = U | D | L | R deriving Show

type Indata = [Movement]
type Vec2 = (Int, Int)

addV2 :: Vec2 -> Vec2 -> Vec2
addV2 (x1,y1) (x2,y2) = (x1+x2,y1+y2)

main :: IO ()
main = do
    indata <- parseData <$> readFile "9.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData = lines >=> parseLine
    where
        parseLine :: String -> [Movement]
        parseLine l = 
            let (d:amount:_) = words l
                d' = case d of
                    "U" -> U
                    "D" -> D
                    "L" -> L
                    "R" -> R
            in replicate (read amount) d'

move :: Vec2 -> Movement -> Vec2
move (x,y) U = (x,y-1)
move (x,y) D = (x,y+1)
move (x,y) L = (x-1,y)
move (x,y) R = (x+1,y)

moveTail :: Vec2 -> Vec2 -> Vec2
moveTail t@(tx,ty) (hx,hy)
    | dx <= 1 && dy <= 1 = t
    | otherwise = (tx + signum (hx - tx), ty + signum (hy - ty))
    where
        dx = abs $ tx - hx
        dy = abs $ ty - hy

updateState :: (Vec2,Vec2) -> Movement -> (Vec2, Vec2)
updateState (h,t) dir = 
    let newH = move h dir
        newT = moveTail t newH
    in  (newH, newT)

updateState' :: [Vec2] -> Movement -> [Vec2]
updateState' (h:t) dir = 
    let newH = move h dir
    in scanl (\h t -> moveTail t h) newH t

part1 :: Indata -> Int
part1 indata = length . nub . fmap snd $ scanl updateState ((0,0),(0,0)) indata

part2 :: Indata -> Int
part2 indata = length . nub . fmap (!! 9) $ scanl updateState' [(0,0) | _ <- [0..9]] indata
