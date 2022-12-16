import Utils

data Insn = Noop | Add Int deriving Show

type Indata = [Insn]

main :: IO ()
main = do
    indata <- parseData <$> readFile "10.in"
    print $ part1 indata
    putStr $ part2 indata

parseData :: String -> Indata
parseData = fmap parseLine . lines
    where
        parseLine :: String -> Insn
        parseLine "noop" = Noop
        parseLine s = let (_:w:_) = words s in Add (read w)

runInsns :: Int -> [Insn] -> [Int]
runInsns _ [] = []
runInsns acc (Noop:is) = acc:runInsns acc is
runInsns acc ((Add v):is) = acc:acc + v:runInsns (acc+v) is

part1 :: Indata -> Int
part1 indata = foldl (+) 0 $ fmap (\p -> signalStrength ran p) [19,59..220]
    where
        ran = 1:runInsns 1 indata
        signalStrength :: [Int] -> Int -> Int
        signalStrength d pos = (d !! pos) * (pos + 1)

part2 :: Indata -> String
part2 indata = unlines . fmap renderLine $ chunksOf 40 ran
    where
        ran = 1:runInsns 1 indata

renderLine :: [Int] -> String
renderLine registerVals = zipWith (\sPos col -> if abs (sPos - col) <= 1 then '#' else ' ') registerVals [0..]
