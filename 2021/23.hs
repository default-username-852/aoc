import Utils
import Graph
import Control.Monad
import qualified Data.Map as M
import Data.Function
import Data.Maybe

type Indata = State

data Creature = A | B | C | D deriving (Show, Eq, Ord)
data State = State (M.Map Int Creature) [[Creature]] deriving (Show, Eq, Ord)

main :: IO ()
main = do
    print $ part1 parseData
    print $ part2 parseData'

moveCost :: Creature -> Int
moveCost A = 1
moveCost B = 10
moveCost C = 100
moveCost D = 1000

creatureHallway :: Creature -> Int
creatureHallway A = 0
creatureHallway B = 1
creatureHallway C = 2
creatureHallway D = 3

neighbourStates :: Int -> State -> [(State, Int)]
neighbourStates depth (State hallway slots) = corridors ++ inserts
    where
        corridors = do
            (cIdx, x) <- zip [0..3] [2,4,6,8]
            let corridor = slots !! cIdx
            guard $ length corridor > 0
            let minX = M.keys hallway & filter (<x) & (-1:) & maximum
            let maxX = M.keys hallway & filter (>x) & (11:) & minimum
            px <- [minX + 1..maxX - 1] & filter (`notElem` [2,4,6,8])
            let cost = moveCost (head corridor) * ((depth-length corridor+1) + abs (x-px))
            let newSlots = let (h, (_:t)) = splitAt cIdx slots in h ++ (tail corridor:t)
            return $ (State (M.insert px (head corridor) hallway) newSlots, cost)
        inserts = do
            (x,creat) <- M.assocs hallway
            let minX = M.keys hallway & filter (<x) & (-1:) & maximum
            let maxX = M.keys hallway & filter (>x) & (11:) & minimum
            px <- [minX..maxX] & filter (==(creatureHallway creat * 2 + 2))
            let cIdx = (px - 2) `div` 2
            let corridor = slots !! cIdx
            guard $ length corridor < depth
            guard $ all (==creat) corridor
            let cost = moveCost creat * ((depth-length corridor) + abs (x-px))
            let newSlots = let (h, (m:t)) = splitAt cIdx slots in h ++ ((creat:m):t)
            return $ (State (M.delete x hallway) newSlots, cost)

parseData :: Indata
parseData = State M.empty [[D,B], [A,C], [D,B], [C,A]]

parseData' :: Indata
parseData' = State M.empty [[D,D,D,B], [A,C,B,C], [D,B,A,B], [C,A,C,A]]

goal :: Indata
goal = State M.empty [[A,A],[B,B],[C,C],[D,D]]

goal' :: Indata
goal' = State M.empty [[A,A,A,A],[B,B,B,B],[C,C,C,C],[D,D,D,D]]

part1 :: Indata -> Int
part1 indata = dijkstra (neighbourStates 2) indata goal & fromJust & snd

part2 :: Indata -> Int
part2 indata = dijkstra (neighbourStates 4) indata goal' & fromJust & snd
