import Utils

data Instruction = Instruction {from, to, amount :: Int} deriving Show

type Indata = [Instruction]

crates :: [[Char]]
crates =
    [ "FLMW"
    , "FMVZB"
    , "QLSRVH"
    , "JTMPQVSF"
    , "WSL"
    , "WJRMPVF"
    , "FRNPCQJ"
    , "BRWZSPHV"
    , "WZHGCJMB"
    ]

main :: IO ()
main = do
    indata <- parseData <$> readFile "5.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData = fmap parseLine . lines
    where
        parseLine l = let (_:c:_:f:_:t:_) = words l in Instruction {from = read f - 1, to = read t - 1, amount = read c}

runInsn :: [[Char]] -> Instruction -> [[Char]]
runInsn stack insn = 
    let modifying = stack !! from insn
        (moving,newFrom) = splitAt (amount insn) modifying
        newTo = reverse moving ++ stack !! to insn
    in modifyAt newTo (to insn) $ modifyAt newFrom (from insn) stack

runInsn' :: [[Char]] -> Instruction -> [[Char]]
runInsn' stack insn = 
    let modifying = stack !! from insn
        (moving,newFrom) = splitAt (amount insn) modifying
        newTo = moving ++ stack !! to insn
    in modifyAt newTo (to insn) $ modifyAt newFrom (from insn) stack

modifyAt :: a -> Int -> [a] -> [a]
modifyAt new idx xs = let (h,t) = splitAt idx xs in h ++ [new] ++ tail t

part1 :: Indata -> String
part1 indata = fmap head $ foldl runInsn crates indata

part2 :: Indata -> String
part2 indata = fmap head $ foldl runInsn' crates indata
