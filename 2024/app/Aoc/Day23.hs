{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Aoc.Day23(solve) where
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable (Foldable(foldl'))
import Aoc.Utils (splitWhen)
import Control.Monad (guard)

type Indata = Graph String

newtype Graph a = Graph
    { graphNeighbours :: M.Map a (S.Set a)
    } deriving Show

insertEdge :: Ord a => Graph a -> (a, a) -> Graph a
insertEdge g (a, b) = g { graphNeighbours = M.unionWith (<>) (M.fromList [(a, S.singleton b), (b, S.singleton a)]) $ graphNeighbours g}

emptyGraph :: Graph a
emptyGraph = Graph M.empty

vertices :: Ord a => Graph a -> S.Set a
vertices = M.keysSet . graphNeighbours

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    part2 parsed

parseData :: String -> Indata
parseData input =
    let pairs = ((\(a:b:_) -> (a,b)) . splitWhen (=='-') <$> lines input)
    in foldl' insertEdge emptyGraph pairs

threeCliques :: Ord a => Graph a -> [(a,a,a)]
threeCliques g = do
    let v = S.toList $ vertices g
    v1 <- v
    v2 <- v
    v3 <- v
    guard $ v1 < v2 && v2 < v3
    guard $ neighbours v1 v2 && neighbours v1 v3 && neighbours v2 v3
    pure (v1,v2,v3)
    where
        neighbours a b = maybe False (b `S.member`) $ M.lookup a (graphNeighbours g)

part1 :: Indata -> Int
part1 indata = length . filter (\(a,b,c) -> 't' `elem` [head a, head b, head c]) $ threeCliques indata

toDIMACS :: (Ord a, Show a) => Graph a -> IO ()
toDIMACS g =
    let n = graphNeighbours g
        numberedNodes = M.fromList $ flip zip [(1 :: Int)..] $ M.keys (graphNeighbours g)
    in do
        print numberedNodes
        putStrLn $ "p edge " <> show (length $ M.keys n) <> " " <> (show . (`div` 2) . sum . fmap S.size $ M.elems n) <> "\n"
            <> (unlines . fmap (\(a,b) -> "e " <> show a <> " " <> show b) . concatMap (\(from, tos) ->
                filter (uncurry (<)) . fmap (\to -> (numberedNodes M.! from, numberedNodes M.! to)) $ S.elems tos) $ M.assocs n)

part2 :: Indata -> IO ()
part2 indata = toDIMACS indata
