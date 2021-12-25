module Graph (bfs, dfs, dijkstra, Graph, WeightedGraph) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Heap as H
import Data.Maybe

type Graph a = M.Map a [a]
type WeightedGraph a = M.Map a [(a, Int)]

dijkstra :: Ord a => (a -> [(a, Int)]) -> a -> a -> Maybe ([a], Int)
dijkstra graph at goal = 
    let (_, _, prevs) = fromJust . last . takeWhile isJust . iterate (>>=dijkstra_ graph) $ Just (H.singleton (0, at), S.empty, M.empty)
        prevNode node = fmap (fst) $ node `M.lookup` prevs
    in
        if goal `M.member` prevs
            then Just (reverse . fmap fromJust . takeWhile isJust . iterate (>>=prevNode) $ Just goal, snd $ prevs M.! goal)
            else Nothing

dijkstra_ :: Ord a => (a -> [(a, Int)]) -> (H.MinPrioHeap Int a, S.Set a, M.Map a (a, Int)) -> Maybe (H.MinPrioHeap Int a, S.Set a, M.Map a (a, Int))
dijkstra_ graph (frontier, visited, prevs) = 
    case H.view frontier of
        Nothing -> Nothing
        Just ((dist, on), frontier') -> if on `S.member` visited
            then Just (frontier', visited, prevs)
            else 
                let neighbours = graph on
                    visited' = S.insert on visited
                    prevs' = foldl (\p (node, nDist) -> M.insertWith (\new@(_, newDist) old@(_, oldDist) -> if newDist < oldDist then new else old) node (on, dist + nDist) p) prevs neighbours
                    frontier'' = foldl (flip H.insert) frontier' $ fmap (\(node, cost) -> (cost + dist, node)) neighbours
                in Just (frontier'', visited', prevs')
                    

bfs :: (Eq a, Ord a) => M.Map a [a] -> a -> a -> Bool
bfs graph at goal = bfs_ graph [at] S.empty goal

bfs_ :: (Eq a, Ord a) => M.Map a [a] -> [a] -> S.Set a -> a -> Bool
bfs_ graph frontier visited goal
    | null frontier = False
    | head frontier == goal = True
    | otherwise = bfs_ graph ((filter (not . flip S.member visited) $ graph M.! head frontier) ++ tail frontier) (S.insert (head frontier) visited) goal

dfs :: (Eq a, Ord a) => M.Map a [a] -> a -> a -> Bool
dfs graph at goal = dfs_ graph [at] S.empty goal

dfs_ :: (Eq a, Ord a) => M.Map a [a] -> [a] -> S.Set a -> a -> Bool
dfs_ graph frontier visited goal
    | null frontier = False
    | head frontier == goal = True
    | otherwise = dfs_ graph (tail frontier ++ (filter (not . flip S.member visited) $ graph M.! head frontier)) (S.insert (head frontier) visited) goal
