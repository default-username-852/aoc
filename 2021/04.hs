import Utils
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Debug.Trace

type Board = [[Int]]
type Indata = ([Int], [Board])

main :: IO ()
main = do
    indata <- parseData <$> readFile "04.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData indata = let (nums:board) = lines indata in (read <$> splitWhen (==',') nums, fmap readBoard $ chunksOf 6 board)
    where
        readBoard :: [String] -> Board
        readBoard (_:indata) = fmap fmap fmap read $ fmap words indata

checkBingo :: S.Set (Int, Int) -> Bool
checkBingo marked = or [(S.fromList [(a,x) | x <- [0..4]]) `S.isSubsetOf` marked || (S.fromList [(x,a) | x <- [0..4]]) `S.isSubsetOf` marked | a <- [0..4]]

findInBoard :: Int -> Board -> Maybe (Int, Int)
findInBoard num b = do
    (row, rowIdx) <- find (\(r, _) -> any (==num) r) $ zip b [0..]
    (_, elemIdx) <- find (\e -> fst e == num) $ zip row [0..]
    return (rowIdx, elemIdx)

markBoard :: Int -> Board -> S.Set (Int, Int) -> S.Set (Int, Int)
markBoard num b marked = case findInBoard num b of
    Just p -> p `S.insert` marked
    Nothing -> marked

sumUnmarked :: S.Set (Int, Int) -> Board -> Int
sumUnmarked marked b = sum [b !! x !! y | x <- [0..4], y <- [0..4], not $ (x, y) `S.member` marked]

part1 :: Indata -> Int
part1 (nums, boards) = let (marked, b) = fromJust $ find (\(m, _) -> checkBingo m) lastBs in calledNum * sumUnmarked marked b
    where
        states :: [(Int, [(S.Set (Int, Int), Board)])]
        states = scanl (\(_, bs) n -> (n, fmap (\(marked, b) -> (markBoard n b marked, b)) bs)) (0, fmap ((,) S.empty) boards) nums
        (calledNum, lastBs) = head $ dropWhile (\(_, bs) -> not $ any (\(marked, _) -> checkBingo marked) bs) states

part2 :: Indata -> Int
part2 (nums, boards) = let (marked, b) = fromJust $ find (\(m, _) -> not $ checkBingo m) lastBs in calledNum * (sumUnmarked marked b - calledNum)
    where
        states :: [(Int, [(S.Set (Int, Int), Board)])]
        states = scanl (\(_, bs) n -> (n, fmap (\(marked, b) -> (markBoard n b marked, b)) bs)) (0, fmap ((,) S.empty) boards) nums
        (calledNum, lastBs) = let (pre, post) = span (\(_, bs) -> not $ all (\(marked, _) -> checkBingo marked) bs) states in (fst $ head post, snd $ last pre)
