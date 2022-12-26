import Utils
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))
import Data.Function
import Data.Maybe
import Graph
import Control.Monad
import Control.Applicative
import Debug.Trace

data Pipe = Pipe {pFlow :: Int, pNeigh :: [String]} deriving Show
type Indata = M.Map String Pipe

main :: IO ()
main = do
    indata <- parseData <$> readFile "16.in"
    --print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData indata = foldl (\m l -> parseLine m l) M.empty $ lines indata
    where
        parseLine :: M.Map String Pipe -> String -> M.Map String Pipe
        parseLine m l =
            let (_:from:_:_:rate:_:_:_:_:to) = words l
            in M.insert from (Pipe {pFlow = read . drop 5 $ init rate, pNeigh = fmap (filter (/=',')) to}) m

part1 :: Indata -> Int
part1 indata = maxFlow indata "AA"

pathBetween :: M.Map String Pipe -> Int -> String -> String -> Maybe [String]
pathBetween graph limit at goal = 
    let path = dijkstra (\n -> graph ! n & pNeigh & fmap (flip (,) 1)) at goal
    in do 
        (p, c) <- path
        guard $ c <= limit
        return p

pathsBetween :: M.Map String Pipe -> String -> Int -> S.Set String -> [[(String, Int)]]
pathsBetween graph at limit avaliable 
    | limit <= 0 = [[]]
    | otherwise = []:
    do
        n <- S.toList avaliable 
        (_:path) <- maybeToList $ pathBetween graph limit at n
        let cost = length path
        fmap ((n, limit - cost - 1):) $ pathsBetween graph n (limit - cost - 1) (S.delete n avaliable)

positiveNodes :: M.Map String Pipe -> [String]
positiveNodes graph = mapMaybe filterPositive $ M.toList graph
    where
        filterPositive (n, Pipe {pFlow = f}) = if f > 0 then Just n else Nothing

maxFlow :: M.Map String Pipe -> String -> Int
maxFlow graph at = maximum $ fmap pathValue $ pathsBetween graph "AA" 30 (S.fromList $ positiveNodes graph)
    where
        nodeFlow :: String -> Int
        nodeFlow n = graph ! n & pFlow
        pathValue :: [(String, Int)] -> Int
        pathValue p = fmap (\(n, t) -> t * nodeFlow n) p & sum

maxFlow' :: 
    M.Map String Int -> 
    M.Map (String, String) Int -> 
    ((String, Int), (String, Int)) -> 
    Int -> 
    Int -> 
    Int -> 
    Int
maxFlow' flows graph _ currentScore _ 0 = currentScore
maxFlow' flows graph ((goal1,time1), (goal2, time2)) currentScore bestYet timeLeft
    | time1 > 0 && time2 > 0 = maxFlow' flows graph ((goal1, time1-1), (goal2, time2-1)) currentScore bestYet (timeLeft - 1)
    | M.elems flows & fmap (\f -> f * (timeLeft-1)) & sum & (+currentScore) & (<=bestYet) = 0
        -- check if there is no way to obtain a better result
    | otherwise = 
        let nonZeroNodes = mapMaybe (\(n, f) -> if f > 0 then Just n else Nothing) $ M.toList flows'
            (moves, updateCapacity, updateScore) = 
                case (time1,time2) of
                    (0,0) -> (
                        [((x,graph ! (goal1,x)),(y,(graph ! (goal2,y)))) | x <- nonZeroNodes, y <- nonZeroNodes, x /= y], 
                        zeroFlow goal1 . zeroFlow goal2,
                        if goal1 == goal2
                        then (+((flows ! goal1)*(timeLeft-1)))
                        else (+((flows ! goal1)*(timeLeft-1))) . (+((flows ! goal2)*(timeLeft-1))))
                    (0,_) -> (
                        [((x,graph ! (goal1,x)),(goal2,time2-1)) | x <- nonZeroNodes], 
                        zeroFlow goal1,
                        (+((flows ! goal1)*(timeLeft-1))))
                    (_,0) -> (
                        [((goal1,time1-1),(y,(graph ! (goal2,y)))) | y <- nonZeroNodes], 
                        zeroFlow goal2,
                        (+((flows ! goal2)*(timeLeft-1))))
                    (_,_) -> undefined -- unreacable by previous guard and invariants
            flows' = updateCapacity flows
            score' = updateScore currentScore
            makeMove prevBest newAt =
                let potFlow = maxFlow' flows' graph newAt score' prevBest (timeLeft-1)
                in max prevBest potFlow
        in if null moves
        then updateScore currentScore -- cant make any moves and all score has been counted, just return
        else foldl makeMove bestYet moves
    where
        zeroFlow :: String -> M.Map String Int -> M.Map String Int
        zeroFlow node = M.adjust (const 0) node

part2 :: Indata -> Int
part2 indata = foldl (\best start -> max best $ maxFlow' flows paths' start 0 best 26) 0 starts
    where
        flows = M.filter (\f -> f > 0) $ fmap pFlow indata
        paths = fromJust <$> floydWarshall (M.keys indata) (\v -> indata ! v & pNeigh & fmap (\n -> (n,1)))
        paths' = M.filterWithKey (\(f,t) _ -> f `M.member` flows && t `M.member` flows) paths
        starts = --[(("JJ",2),("DD",1))]
            [((x,paths ! ("AA",x)),(y,paths ! ("AA", y))) | x <- M.keys flows, y <- M.keys flows, x /= y]
