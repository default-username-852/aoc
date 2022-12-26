module Graph (dijkstra, Graph, WeightedGraph, floydWarshall) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.PSQueue as PSQ
import Data.PSQueue (Binding((:->)))
import Data.Maybe
import Control.Monad
import Control.Monad.State.Lazy

type Graph a = M.Map a [a]
type WeightedGraph a = M.Map a [(a, Int)]

dijkstra :: (Ord a, Show a) => (a -> [(a, Int)]) -> a -> a -> Maybe ([a], Int)
dijkstra graph at goal = 
    let (_, _, prevs) = fromJust . last . takeWhile isJust . iterate (>>=dijkstra_ graph goal) $ Just (PSQ.singleton at 0, S.empty, M.empty)
        prevNode node = fmap fst $ node `M.lookup` prevs
    in
        if goal `M.member` prevs
            then Just (reverse . fmap fromJust . takeWhile isJust . iterate (>>=prevNode) $ Just goal, snd $ prevs M.! goal)
            else Nothing

dijkstra_ :: (Ord a, Show a) => 
      (a -> [(a, Int)]) 
    -> a 
    ->       (PSQ.PSQ a Int, S.Set a, M.Map a (a, Int)) 
    -> Maybe (PSQ.PSQ a Int, S.Set a, M.Map a (a, Int))
dijkstra_ graph goal (frontier, visited, prevs) = 
    case PSQ.minView frontier of
        Nothing -> Nothing
        Just (on:->dist, frontier') -> if on == goal
            then Nothing
            else 
                let neighbours = filter (\(n,_) -> not (n `S.member` visited)) $ graph on
                    visited' = S.insert on visited
                    prevs' = foldl (\p (node, nDist) -> M.insertWith (\new@(_, newDist) old@(_, oldDist) -> if newDist < oldDist then new else old) node (on, dist + nDist) p) prevs neighbours
                    alterPrio :: Int -> Maybe Int -> Maybe Int
                    alterPrio c Nothing = Just c
                    alterPrio c (Just v) = Just $ min c v
                    frontier'' = foldl (\f (d, n) -> PSQ.alter (alterPrio d) n f) frontier' $ fmap (\(node, cost) -> (cost + dist, node)) neighbours
                in Just (frontier'', visited', prevs')

-- return value emulates matrix
floydWarshall :: (Ord a) => [a] -> (a -> [(a, Int)]) -> M.Map (a, a) (Maybe Int)
floydWarshall vertices edges = execState (forM_ vertices $ \k -> forM_ vertices $ \i -> forM_ vertices $ \j -> updateState k i j) initialMap
    where
        extendEdges nonInf = 
            let vertMap = M.fromList nonInf
            in fmap (\v -> (v, M.lookup v vertMap)) vertices
        initialMap = M.fromList $ (\v -> fmap (\(t, c) -> ((v,t), c)) . extendEdges . ((v,0):) $ edges v) =<< vertices
        updateState :: Ord a => a -> a -> a -> State (M.Map (a, a) (Maybe Int)) ()
        updateState k i j = modify $ 
            \mat -> M.adjust (
                \d -> minMaybe d $ pure (+) <*> mat M.! (i,k) <*> mat M.! (k,j)
                ) (i,j) mat
        minMaybe :: Maybe Int -> Maybe Int -> Maybe Int
        minMaybe (Just a) (Just b) = Just $ min a b
        minMaybe a Nothing = a
        minMaybe Nothing b = b
