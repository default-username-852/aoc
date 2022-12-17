import Utils
import Data.List
import Prelude hiding (round)

type Indata = [Monkey]

data Monkey = Monkey {mItems :: ![Integer], mOp :: Integer -> Integer, mThrowTo :: Integer -> Int, mInspections :: !Integer}

instance Show Monkey where
    show m = show $ mItems m

testIndata :: Indata
testIndata = 
    [ Monkey {mItems = [79,98], mOp = (*19), mThrowTo = \v -> if v `mod` 23 == 0 then 2 else 3, mInspections = 0}
    , Monkey {mItems = [54,65,75,74], mOp = (+6), mThrowTo = \v -> if v `mod` 19 == 0 then 2 else 0, mInspections = 0}
    , Monkey {mItems = [79,60,97], mOp = (\v->v*v), mThrowTo = \v -> if v `mod` 13 == 0 then 1 else 3, mInspections = 0}
    , Monkey {mItems = [74], mOp = (+3), mThrowTo = \v -> if v `mod` 17 == 0 then 0 else 1, mInspections = 0}
    ]

realIndata :: Indata
realIndata = 
    [ Monkey [65,78] (*3) (\v -> if v `mod` 5 == 0 then 2 else 3) 0
    , Monkey [54,78,86,79,73,64,85,88] (+8) (\v -> if v `mod` 11 == 0 then 4 else 7) 0
    , Monkey [69,97,77,88,87] (+2) (\v -> if v `mod` 2 == 0 then 5 else 3) 0
    , Monkey [99] (+4) (\v -> if v `mod` 13 == 0 then 1 else 5) 0
    , Monkey [60,57,52] (*19) (\v -> if v `mod` 7 == 0 then 7 else 6) 0
    , Monkey [91,82,85,73, 84, 53] (+5) (\v -> if v `mod` 3 == 0 then 4 else 1) 0
    , Monkey [88,74,68,56] (\v -> v*v) (\v -> if v `mod` 17 == 0 then 0 else 2) 0
    , Monkey [54,82,72,71,53,99,67] (+1) (\v -> if v `mod` 19 == 0 then 6 else 0) 0
    ]

main :: IO ()
main = do
    let indata = realIndata
    print $ part1 indata
    print $ part2 indata

updateMonkeyWorry :: (Integer -> Integer) -> Monkey -> Monkey
updateMonkeyWorry dFunc m@Monkey{mItems = items, mInspections = inspections} = 
    m {mItems = fmap (dFunc . mOp m) items, mInspections = inspections + fromIntegral (length items)}

monkeyThrow :: Monkey -> [(Int, Integer)]
monkeyThrow m = fmap (\i -> (mThrowTo m i,i)) $ mItems m

updateMonkeys :: [(Int, Integer)] -> [Monkey] -> [Monkey]
updateMonkeys items monkeys = foldl (\ms (to, item) -> modifyAt (updateMonkey item) to ms) monkeys items
    where
        updateMonkey item monkey = monkey {mItems = mItems monkey ++ [item]}

round :: (Integer -> Integer) -> [Monkey] -> [Monkey]
round dFunc monkeys = foldl monkeyRound monkeys [0..length monkeys - 1]
    where
        monkeyRound :: [Monkey] -> Int -> [Monkey]
        monkeyRound monkeys idx = 
            let monkey = updateMonkeyWorry dFunc $ monkeys !! idx
                itemPoss = monkeyThrow monkey
            in modifyAt (\_ -> monkey {mItems = []}) idx $ updateMonkeys itemPoss monkeys

part1 :: Indata -> Integer
part1 indata = let (a:b:_) = take 2 . reverse . sort . fmap mInspections . (!! 20) $ iterate (round (`div` 3)) indata
    in a*b

part2 :: Indata -> Integer
part2 indata = let (a:b:_) = take 2 . reverse . sort . fmap mInspections . (!! 10000) $ iterate (round (`mod` p)) indata
    in a*b
    where
        p :: Integer
        p = 5*11*2*13*7*3*17*19

