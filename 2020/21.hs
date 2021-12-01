import Utils
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe

main :: IO ()
main = do
    indata <- readFile "21.in"
    putStrLn $ parts indata

parts :: String -> String
parts indata = res1 ++ "\n" ++ res2
    where
        parsed = parseData indata
        merged = uncurry (foldl (M.unionWith S.intersection)) $ fromJust $ uncons $ map (\(ingreds, algens) -> M.fromList $ zip algens (repeat ingreds)) parsed
        allergens = foldl (S.union) S.empty merged
        res1 = show $ length $ filter (not . flip S.member allergens) $ (S.elems . fst) =<< parsed
        res2 = intercalate "," $ map snd $ sortOn fst $ head $ solvePossibilities $ M.assocs merged

parseData :: String -> [(S.Set String, [String])]
parseData indata = map ((\(ingreds:allergens:_) -> (S.fromList ingreds, map init allergens)) . splitWhen (=="(contains") . words) $ lines indata

solvePossibilities :: [(String, S.Set String)] -> [[(String, String)]]
solvePossibilities [] = [[]]
solvePossibilities ((key, vals):rest) = do
    val <- S.elems vals
    let rest' = map (\(k, vs) -> (k, S.delete val vs)) rest
    possibility <- solvePossibilities rest'
    return ((key, val):possibility)
