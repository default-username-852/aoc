import Utils
import Text.Parsec
import Text.Parsec.String
import qualified Data.Set as S

type Indata = [[Vec2]]
type Vec2 = (Int, Int)

lineSegment :: Parser [Vec2]
lineSegment = 
    sepBy1 point 
        (  spaces
        >> char '-'
        >> char '>'
        >> spaces)

point :: Parser Vec2
point = do
    x <- read <$> many1 digit
    _ <- char ','
    y <- read <$> many1 digit
    return (x,y)

main :: IO ()
main = do
    indata <- parseData <$> readFile "14.in"
    print $ part1 indata
    print $ part2 indata

segmentToPoints :: [Vec2] -> S.Set Vec2
segmentToPoints l = S.unions . fmap segmentToPoint $ windows 2 l
    where
        segmentToPoint :: [Vec2] -> S.Set Vec2
        segmentToPoint ((x1,y1):(x2,y2):_) = 
            S.fromList [(x,y) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2]]
        segmentToPoint _ = error "uh oh stinky"


parseData :: String -> Indata
parseData = fmap (\l -> let (Right v) = parse lineSegment "foo" l in v) . lines

dropSand :: Int -> (Vec2 -> Maybe Vec2) -> S.Set Vec2 -> Vec2 -> Maybe Vec2
dropSand boundary onFloor walls p@(x,y)
    | y >= boundary = onFloor p
    | not ((x  ,y+1) `S.member` walls) = dropSand' (x,y+1)
    | not ((x-1,y+1) `S.member` walls) = dropSand' (x-1,y+1)
    | not ((x+1,y+1) `S.member` walls) = dropSand' (x+1,y+1)
    | otherwise = Just p
    where
        dropSand' = dropSand boundary onFloor walls

part1 :: Indata -> Int
part1 indata = sands 500 (const Nothing) (S.unions $ fmap segmentToPoints indata) S.empty

sands :: Int -> (Vec2 -> Maybe Vec2) -> S.Set Vec2 -> S.Set Vec2 -> Int
sands boundary onFloor walls s
    | (500,0) `S.member` (S.union walls s) = 0
    | otherwise = 
        case dropSand boundary onFloor (S.union walls s) (500,0) of
            Just p -> 
                let sands' = S.insert p s
                in  sands boundary onFloor walls sands' + 1
            Nothing -> 0

part2 :: Indata -> Int
part2 indata = sands ((+1) . maximum . fmap snd $ S.elems walls) Just walls S.empty
    where
        walls = S.unions $ fmap segmentToPoints indata
