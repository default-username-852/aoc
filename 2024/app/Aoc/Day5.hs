{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Aoc.Day5(solve) where
import Aoc.Utils
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Foldable (Foldable(foldr'))
import Data.List
import Data.Maybe (fromJust, fromMaybe)

type Indata = (M.Map Int (S.Set Int), [[Int]])

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

parseData :: String -> Indata
parseData indata =
    let (rules:updates:_) = splitWhen (=="") $ lines indata
        addRule :: M.Map Int (S.Set Int) -> (Int, Int) -> M.Map Int (S.Set Int)
        addRule r (before, after) = M.insertWith S.union before (S.singleton after) r
        parsedRules = foldr' (flip addRule) M.empty $ fmap (\r -> let (a:b:_) = splitWhen (=='|') r in (read a, read b)) rules
        parsedUpdates = fmap (fmap read . splitWhen (== ',')) updates
    in (parsedRules, parsedUpdates)

unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

correctOrder :: M.Map Int (S.Set Int) -> [Int] -> Bool
correctOrder rules prints =
    all ((\(preds, current) ->
        let afters = fromMaybe S.empty $ M.lookup current rules
        in not (any (`S.member` afters) preds)) . fromJust . unsnoc) $ tail (inits prints)

part1 :: Indata -> Int
part1 (rules, updates) = sum . fmap (\printJob -> printJob !! (length printJob `div` 2)) $ filter (correctOrder rules) updates

comesBefore :: M.Map Int (S.Set Int) -> Int -> Int -> Bool
comesBefore rules before after = S.member after . fromMaybe S.empty $ M.lookup before rules

fixOrdering :: M.Map Int (S.Set Int) -> [Int] -> [Int]
fixOrdering rules = sortBy (\a b -> if comesBefore rules a b then LT else if comesBefore rules b a then GT else EQ)

part2 :: Indata -> Int
part2 (rules,updates) = sum . fmap ((\printJob -> printJob !! (length printJob `div` 2)) . fixOrdering rules) $ filter (not . correctOrder rules) updates
