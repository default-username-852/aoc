{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Aoc.Utils (windows, sublists, subsets, splitWhen, chunksOf, modifyAt, insertAt) where

import qualified Data.List as L
import Prelude hiding (pred)

--binarySplit :: (Int, Int) -> Bool -> (Int, Int)
--binarySplit (lower, upper) choose = if choose then (ceiling (fromIntegral (upper + lower) / 2), upper) else (lower, div (upper + lower) 2)

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . L.tails

sublists :: [a] -> [[a]]
sublists = (:) [] . concatMap (filter (not . null) . L.tails) . L.inits

subsets :: [Int] -> [[Int]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

splitWhen :: (t -> Bool) -> [t] -> [[t]]
splitWhen pred lst
    | null lst = [[]]
    | pred $ head lst = []:splitWhen pred (tail lst)
    | otherwise = (head lst:begin):end
    where
        (begin:end) = splitWhen pred $ tail lst

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
    | n > 0 = take n l : chunksOf n (drop n l)
    | otherwise = error "Negative or zero n"

insertAt :: a -> Int -> [a] -> [a]
insertAt new idx xs = let (h,t) = splitAt idx xs in h ++ [new] ++ tail t

modifyAt :: (a -> a) -> Int -> [a] -> [a]
modifyAt f idx xs = let (h,t:ts) = splitAt idx xs in h ++ [f t] ++ ts
