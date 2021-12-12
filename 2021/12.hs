import Utils
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char
import Data.List
import Control.Monad

type Indata = M.Map String [String]

main :: IO ()
main = do
    indata <- parseData <$> readFile "12.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData indata = foldl parseLine M.empty $ lines indata
    where
        parseLine :: M.Map String [String] -> String -> M.Map String [String]
        parseLine m l = let (t:f:_) = splitWhen (=='-') l in M.insertWith (++) t [f] $ M.insertWith (++) f [t] m

paths :: M.Map String [String] -> String -> S.Set String -> Int
paths _ "start" _ = 1
paths m on visited = sum $ do
    n <- filter (flip S.notMember visited) (m M.! on)
    return $ paths m n (if isUpper $ head on then visited else S.insert on visited)

paths' :: M.Map String [String] -> String -> S.Set String -> Int
paths' _ "start" _ = 1
paths' m on visited = sum $ 
    (do
        n <- filter (flip S.member visited) (m M.! on)
        guard $ n /= "end"
        return $ paths m n (if isUpper $ head on then visited else S.insert on visited))
    ++ (do
        n <- filter (flip S.notMember visited) (m M.! on)
        return $ paths' m n (if isUpper $ head on then visited else S.insert on visited))

part1 :: Indata -> Int
part1 indata = paths indata "end" S.empty

part2 :: Indata -> Int
part2 indata = paths' indata "end" S.empty
