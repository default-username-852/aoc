module Utils (binarySplit, windows, sublists, splitWhen, chunksOf) where

import qualified Data.List as L

binarySplit :: (Int, Int) -> Bool -> (Int, Int)
binarySplit (lower, upper) choose = if choose then (ceiling (fromIntegral (upper + lower) / 2), upper) else (lower, div (upper + lower) 2)

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . L.tails

sublists :: [a] -> [[a]]
sublists = (:) [] . filter (not . null) . concat . map L.tails . L.inits

splitWhen :: (t -> Bool) -> [t] -> [[t]]
splitWhen pred lst
    | null lst = [[]]
    | pred $ head lst = []:(splitWhen pred $ tail lst)
    | otherwise = (head lst:begin):end
    where
        (begin:end) = splitWhen pred $ tail lst

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
    | n > 0 = (take n l) : (chunksOf n (drop n l))
    | otherwise = error "Negative or zero n"
