import Utils
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Control.Monad
import Debug.Trace

main :: IO ()
main = do
    indata <- readFile "19.in"
    print $ part1 indata
    indata' <- readFile "19b.in"
    print $ part1 indata'

part1 :: String -> Int
part1 indata = let (msgs, rules) = parseData indata in length $ filter (elem "") $ map (rules M.! 0) msgs

parseData :: String -> ([String], M.Map Int (String -> [String]))
parseData indata = (msgs, compRules)
    where
        (rulesStr:msgs:_) = splitWhen (=="") $ lines indata
        parsedRules = do
            ruleStr <- rulesStr
            let (num, rest) = span (/=' ') ruleStr
            let num' = read $ init num
            let func = parsePattern compRules (tail rest)
            return (num', func)
        compRules = M.fromList parsedRules

parsePattern :: M.Map Int (String -> [String]) -> String -> String -> [String]
parsePattern _ ('"':c:'"':_) (l:rest) = if l == c then [rest] else []
parsePattern _ ('"':c:'"':_) [] = []
parsePattern pats pat matching = join $ map (foldl (>>=) ([matching])) $ map (map ((M.!) pats . read) . words) $ splitWhen (=='|') pat
