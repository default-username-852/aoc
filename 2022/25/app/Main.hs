import Utils
import Control.Monad.State.Lazy

type Indata = [String]

main :: IO ()
main = do
    indata <- parseData <$> readFile "25.in"
    putStrLn $ part1 indata

parseData :: String -> Indata
parseData = lines

snafuToDec :: String -> Int
snafuToDec s = foldl (\acc e -> acc * 5 + snafuDigit e) 0 s

snafuDigit :: Char -> Int
snafuDigit '2' = 2
snafuDigit '1' = 1
snafuDigit '0' = 0
snafuDigit '-' = -1
snafuDigit '=' = -2

digitInv :: Int -> Char
digitInv 2 = '2'
digitInv 1 = '1'
digitInv 0 = '0'
digitInv (-1) = '-'
digitInv (-2) = '='

extendLists :: String -> String -> [(Char, Char)]
extendLists (a:as) (b:bs) = (a,b):extendLists as bs
extendLists [] (b:bs) = ('0', b):extendLists [] bs
extendLists (a:as) [] = (a, '0'):extendLists as []
extendLists [] [] = []

addSnafu :: String -> String -> String
addSnafu a b = 
    let (v, c) = flip runState '0' . fmap reverse . mapM addOnes $ extendLists (reverse a) (reverse b)
    in if c /= '0'
    then c:v
    else v
    where
        addOnes :: (Char, Char) -> State Char Char
        addOnes (a,b) = do
            carry <- get
            let pSum = snafuDigit a + snafuDigit b + snafuDigit carry
            if pSum >= -2 && pSum <= 2
            then put '0' >> return (digitInv pSum)
            else if pSum > 2
            then put '1' >> return (digitInv $ pSum - 5)
            else put '-' >> return (digitInv $ pSum + 5)


part1 :: Indata -> String
part1 indata = foldl addSnafu "0" indata
