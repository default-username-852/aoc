import Utils
import qualified Data.Map as M
import Data.List

type Indata = (String, M.Map String [String])

main :: IO ()
main = do
    indata <- parseData <$> readFile "14.in"
    print $ part 10 indata
    print $ part 40 indata

parseData :: String -> Indata
parseData indata = let (p:_:rules) = lines indata in (p, foldl (M.union) M.empty $ fmap readRule rules)
    where
        readRule l = let (a:b:_:_:_:_:t:_) = l in M.singleton (a:b:[]) [[a,t], [t,b]]

polymerize' :: M.Map String [String] -> M.Map String Int -> M.Map String Int
polymerize' rules pairs = M.fromListWith (+) $ (\(k,v) -> fmap (flip (,) v) (rules M.! k)) =<< M.toList pairs

part :: Int -> Indata -> Int
part iters (polymer, rules) = let p = M.insertWith (+) (last polymer) 1 . M.mapKeysWith (+) head . (!! iters) . iterate (polymerize' rules) . M.fromListWith (+) . fmap (flip (,) 1) $ windows 2 polymer in maximum p - minimum p
