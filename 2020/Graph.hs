module Graph (bfs, dfs) where

import qualified Data.Map as M
import qualified Data.Set as S

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
