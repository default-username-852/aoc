import Utils

data Throw = Rock | Paper | Scissors deriving (Show, Enum)

type Indata = [(String,String)]

main :: IO ()
main = do
    indata <- parseData <$> readFile "2.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData = fmap ((\(a:b:_) -> (a,b)) . words) . lines

part1 :: Indata -> Int
part1 = sum . fmap scoreRound

part2 :: Indata -> Int
part2 = sum . fmap scoreRound'

scoreRound :: (String,String) -> Int
scoreRound (a,b)
    | a == "A" && b == "X"
        || a == "B" && b == "Y"
        || a == "C" && b == "Z"
        = 3 + throwVal b
    | a == "A" && b == "Y" 
        || a == "B" && b == "Z" 
        || a == "C" && b == "X" 
        = 6 + throwVal b
    | otherwise = throwVal b

scoreRound' :: (String,String) -> Int
scoreRound' (a,b) = throwVal' (shapeToResult t b) + roundScore b
    where
        t = readRPS a

nextThrow :: Throw -> Throw
nextThrow Scissors = Rock
nextThrow a = succ a

prevThrow :: Throw -> Throw
prevThrow Rock = Scissors
prevThrow a = pred a

readRPS :: String -> Throw
readRPS "A" = Rock
readRPS "B" = Paper
readRPS "C" = Scissors

shapeToResult :: Throw -> String -> Throw
shapeToResult t "X" = prevThrow t
shapeToResult t "Y" = t
shapeToResult t "Z" = nextThrow t

throwVal :: String -> Int
throwVal "X" = 1
throwVal "Y" = 2
throwVal "Z" = 3

roundScore :: String -> Int
roundScore "X" = 0
roundScore "Y" = 3
roundScore "Z" = 6

throwVal' :: Throw -> Int
throwVal' Rock = 1
throwVal' Paper = 2
throwVal' Scissors = 3
