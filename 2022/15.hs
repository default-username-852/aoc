import Utils
import Debug.Trace
import Data.Maybe

type Indata = [Sensor]
type Vec2 = (Int, Int)

data Sensor = Sensor { sPos :: Vec2, bPos :: Vec2 } deriving Show
data Rect = Rect { rCorner :: Vec2, rWidth, rHeight :: Int } deriving Show

testIndata :: Indata
testIndata = fmap (uncurry Sensor)
    [ ((2,18), (-2,15))
    , ((9,16), (10,16))
    , ((13,2), (15,3))
    , ((12,14), (10,16))
    , ((10,20), (10,16))
    , ((14,17), (10,16))
    , ((8,7), (2,10))
    , ((2,0), (2,10))
    , ((0,11), (2,10))
    , ((20,14), (25,17))
    , ((17,20), (21,22))
    , ((16,7), (15,3))
    , ((14,3), (15,3))
    , ((20,1), (15,3))
    ]

realIndata :: Indata
realIndata = fmap (uncurry Sensor)
    [ ((2483411, 3902983), (2289579, 3633785))
    , ((3429446, 303715), (2876111, -261280))
    , ((666423, 3063763), (2264411, 2779977))
    , ((3021606, 145606), (2876111, -261280))
    , ((2707326, 2596893), (2264411, 2779977))
    , ((3103704, 1560342), (2551409, 2000000))
    , ((3497040, 3018067), (3565168, 2949938))
    , ((1708530, 855013), (2551409, 2000000))
    , ((3107437, 3263465), (3404814, 3120160))
    , ((2155249, 2476196), (2264411, 2779977))
    , ((3447897, 3070850), (3404814, 3120160))
    , ((2643048, 3390796), (2289579, 3633785))
    , ((3533132, 3679388), (3404814, 3120160))
    , ((3683790, 3017900), (3565168, 2949938))
    , ((1943208, 3830506), (2289579, 3633785))
    , ((3940100, 3979653), (2846628, 4143786))
    , ((3789719, 1225738), (4072555, 1179859))
    , ((3939775, 578381), (4072555, 1179859))
    , ((3880152, 3327397), (3404814, 3120160))
    , ((3280639, 2446475), (3565168, 2949938))
    , ((2348869, 2240374), (2551409, 2000000))
    , ((3727441, 2797456), (3565168, 2949938))
    , ((3973153, 2034945), (4072555, 1179859))
    , ((38670, 785556), (311084, -402911))
    , ((3181909, 2862960), (3565168, 2949938))
    , ((3099490, 3946226), (2846628, 4143786))
    ]

main :: IO ()
main = do
    let indata = realIndata
    print $ part1 indata
    print $ part2 indata

noBeacon :: [Sensor] -> Vec2 -> Bool
noBeacon sensors pos = (or $ fmap (flip sensorCovers pos) sensors)
    where
        beacons = fmap bPos sensors

sensorCovers :: Sensor -> Vec2 -> Bool
sensorCovers (Sensor {sPos = s, bPos = b}) pos = taxicabDist s pos <= taxicabDist s b

taxicabDist :: Vec2 -> Vec2 -> Int
taxicabDist (ax,ay) (bx,by)  = abs (ax-bx) + abs (ay-by)

removeRect :: Rect -> Rect -> [Rect]
removeRect 
    (Rect {rCorner = (kx,ky), rWidth = kw, rHeight = kh}) 
    (Rect {rCorner = (rx,ry), rWidth = rw, rHeight = rh}) = 
    let topRect = Rect {rCorner = (kx,ky), rWidth = kw, rHeight = ry - ky}
        leftRect = Rect {rCorner = (kx,max ry ky), rWidth = rx - kx, rHeight = middleHeight}
        middleHeight = 
            minimum [rh, rh + ry - ky, ky + kh - ry, kh]
        rightRect = Rect {rCorner = (rx+rw,max ry ky), rWidth = kw - (rWidth leftRect + rw), rHeight = middleHeight}
        bottomRect = Rect {rCorner = (kx,ry+rh), rWidth = kw, rHeight = kh - (rHeight topRect + rh)}
        positiveArea :: Rect -> Bool
        positiveArea (Rect {rWidth = w, rHeight = h}) = w > 0 && h > 0
    in
        if     (kx >= rx + rw || rx >= kx + kw) 
            || (ky >= ry + rh || ry >= ky + kh)
            -- check if disjoint
        then [Rect {rCorner = (kx,ky), rWidth = kw, rHeight = kh}]
        else filter positiveArea [topRect, leftRect, rightRect, bottomRect]

rectPoints :: Rect -> [Vec2]
rectPoints (Rect {rCorner = (x,y), rWidth = w, rHeight = h}) = [(x+dx,y+dy) | dx <- [0..w], dy <- [0..h]]

scannerToRect :: Sensor -> Rect
scannerToRect (Sensor {sPos = s@(sx,sy), bPos = b}) = 
    Rect {rCorner = (tx - ty, tx + ty), rWidth = r * 2, rHeight = r * 2} -- times 2 since matrix has determinant 2
    where
        tx = sx - r
        ty = sy
        r = taxicabDist s b

part1 :: Indata -> Int
part1 indata = length $ filter (\p -> noBeacon indata p) [(x,y) | let y = 2000000, x <- [minX-largestDelta*4..maxX+largestDelta*4]]
    where
        largestDelta = maximum $ fmap (\(Sensor {sPos = s, bPos = b}) -> taxicabDist s b) indata
        minX = minimum $ fmap (\(Sensor {sPos = (x,_)}) -> x) indata
        maxX = maximum $ fmap (\(Sensor {sPos = (x,_)}) -> x) indata

transformBack :: Vec2 -> Maybe Vec2
transformBack (x,y) = 
    let (newX, r) = (x+y) `divMod` 2
        (newY, r2) = (-x+y) `divMod` 2
    in 
        if r == 0 && r2 == 0 then Just (newX,newY) else Nothing

part2 :: Indata -> Int
part2 indata = 
    let rects = fmap scannerToRect indata
        keep = foldl (\keep remove -> keep >>= flip removeRect remove) 
            [Rect {rCorner = (-4000000,0), rWidth = 4000000, rHeight = 2 * 4000000}] rects
        allPoints = rectPoints =<< filter (\Rect {rWidth = w, rHeight = h} -> h <= 5 && w <= 5) keep
        (x,y) = head . filter (\p -> not $ noBeacon indata p) . filter (\(x,y) -> x >= 0 && x <= 4000000 && y >= 0 && y <= 4000000) . mapMaybe transformBack $ allPoints
    in x * 4000000 + y
