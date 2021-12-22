import Utils
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Either
import Data.Maybe
import Control.Monad

type Indata = [SnailNum]

data SnailNum = Leaf Int | Branch SnailNum SnailNum deriving (Show, Eq)
data Dir = L | R deriving (Show, Eq)

type Walk = [Dir]

main :: IO ()
main = do
    indata <- fmap parseData $ readFile "18.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData indata = fmap (fromRight undefined . runParser snailNum () "") $ lines indata

snailNum :: Parser SnailNum
snailNum = choice [ leaf
                  , branch
                  ]

leaf :: Parser SnailNum
leaf = fmap (Leaf . read) $ many1 digit

branch :: Parser SnailNum
branch = do
    char '['
    n1 <- snailNum
    char ','
    n2 <- snailNum
    char ']'
    return $ Branch n1 n2

findNested :: Int -> SnailNum -> [Walk]
findNested _ (Leaf _) = []
findNested 0 (Branch _ _) = [[]]
findNested n (Branch l r) = fmap (L:) (findNested (n-1) l) ++ fmap (R:) (findNested (n-1) r)

leftOf :: Walk -> Maybe Walk
leftOf w = case dropWhile (==L) $ reverse w of
    [] -> Nothing
    (R:xs) -> Just $ reverse (L:xs) ++ repeat R

rightOf :: Walk -> Maybe Walk
rightOf w = case dropWhile (==R) $ reverse w of
    [] -> Nothing
    (L:xs) -> Just $ reverse (R:xs) ++ repeat L

numAt :: Walk -> SnailNum -> SnailNum
numAt (L:xs) (Branch n _) = numAt xs n
numAt (R:xs) (Branch _ n) = numAt xs n
numAt [] n = n

modifyAt :: Walk -> (SnailNum -> SnailNum) -> SnailNum -> SnailNum
modifyAt [] f n = f n
modifyAt (L:xs) f (Branch l r) = Branch (modifyAt xs f l) r
modifyAt (R:xs) f (Branch l r) = Branch l (modifyAt xs f r)
modifyAt _ f l@(Leaf _) = f l

findIn :: (Int -> Bool) -> SnailNum -> [Walk]
findIn f (Leaf n) = if f n then [[]] else []
findIn f (Branch l r) = fmap (L:) (findIn f l) ++ fmap (R:) (findIn f r)

explode :: SnailNum -> Maybe SnailNum
explode n = case findNested 4 n of
    [] -> Nothing
    (x:_) -> 
        let (Branch (Leaf a) (Leaf b)) = numAt x n
            addA (Leaf v) = Leaf (v + a)
            addB (Leaf v) = Leaf (v + b)
            leftAdder = case leftOf x of
                Nothing -> id
                Just p -> modifyAt p addA
            rightAdder = case rightOf x of
                Nothing -> id
                Just p -> modifyAt p addB
        in Just . rightAdder . leftAdder $ modifyAt x (const $ Leaf 0) n

split :: SnailNum -> Maybe SnailNum
split n = case findIn (>=10) n of
    [] -> Nothing
    (x:_) -> 
        let (Leaf v) = numAt x n
        in Just $ modifyAt x (let (d,r) = v `divMod` 2 in const $ Branch (Leaf d) (Leaf $ d + r)) n

step :: SnailNum -> Maybe SnailNum
step n = case explode n of
    Nothing -> split n
    Just v -> Just v

reduceNum :: SnailNum -> SnailNum
reduceNum n = fromJust . last . takeWhile (isJust) . iterate (>>=step) $ Just n

magnitude :: SnailNum -> Int
magnitude (Leaf v) = v
magnitude (Branch l r) = magnitude l * 3 + magnitude r * 2

part1 :: Indata -> Int
part1 indata = magnitude $ foldl1 (\a b -> reduceNum $ Branch a b) indata

part2 :: Indata -> Int
part2 indata = maximum $ do
    n1 <- indata
    n2 <- indata
    return . magnitude . reduceNum $ Branch n1 n2
