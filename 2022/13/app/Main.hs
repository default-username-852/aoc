import Utils
import Text.Parsec
import Text.Parsec.String
import Data.List

type Indata = [(NList, NList)]

data NList = Num Int | List [NList] deriving (Show, Eq)

number :: Parser NList
number = do
    nums <- many1 digit
    return . Num $ read nums

list :: Parser NList
list = do
    between (char '[') (char ']') 
        (do
            children <- sepBy (try number <|> list) (char ',')
            return $ List children)

parseList :: String -> NList
parseList s = case parse list "foo" s of
    Left e -> error $ show e
    Right r -> r

main :: IO ()
main = do
    indata <- parseData <$> readFile "13.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData indata = fmap (\(a:b:_) -> (parseList a, parseList b)) . splitWhen (=="") $ lines indata

compareNL :: NList -> NList -> Ordering
compareNL (Num a) (Num b) = a `compare` b
compareNL (List a) (List b) = 
    case mconcat $ zipWith compareNL a b of
        LT -> LT
        EQ -> length a `compare` length b
        GT -> GT
compareNL la@(List _) b@(Num _) = compareNL la (List [b])
compareNL a@(Num _) lb@(List _) = compareNL (List [a]) lb

part1 :: Indata -> Int
part1 indata = sum . fmap (snd) . filter (\((a,b), _) -> compareNL a b == LT) $ zip indata [1..]

part2 :: Indata -> Int
part2 indata = 
    let sorted = sortBy compareNL indata'
        Just div1 = (+1) <$> elemIndex (List [List [Num 2]]) sorted
        Just div2 = (+1) <$> elemIndex (List [List [Num 6]]) sorted
    in div1 * div2
    where
        indata' = (indata >>= (\(a,b) -> [a,b])) ++ [List [List [Num 2]],List [List [Num 6]]]
