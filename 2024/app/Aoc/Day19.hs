{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections #-}
module Aoc.Day19(solve) where
import Control.Monad.Trans.State.Strict (State, put, get, evalState)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Foldable (find)
import Control.Monad (replicateM)
import Aoc.Utils (windows)

type Indata = ([String], [String])

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

parseData :: String -> Indata
parseData input =
    let (w:_:goals) = lines input
    in (filter (/=',') <$> words w, goals)

data NFA b a = NFA
    { startState :: !a
    , endState :: !a
    , states :: !(S.Set a)
    , transition :: !(a -> [(a, b)])
    }

instance (Show a, Show b, Ord a) => Show (NFA b a) where
    show nfa =
        "NFA { startState = " <> show (startState nfa) <>
        ", endState = " <> show (endState nfa) <>
        ", transitions = " <> show (M.fromList $ fmap (\s -> (s, transition nfa s)) (S.toList $ states nfa)) <>
        " }"

newState :: State Int Int
newState = do
    n <- get
    put (n+1)
    pure n

mkNFA :: [[a]] -> State Int (NFA a Int)
mkNFA ws = do
    ss <- newState
    es <- newState
    subNFAs <- mapM (mkSubNFA ss es) ws
    let t state = if state == ss
        then concatMap (`transition` state) subNFAs
        else
            let nfa = fromJust $ find ((state `S.member`) . states) subNFAs
            in transition nfa state
    pure $ NFA ss es (mconcat $ fmap states subNFAs) t
    where
        mkSubNFA :: Int -> Int -> [a] -> State Int (NFA a Int)
        mkSubNFA ss es word = do
            allStates <- (++[es]) . (ss:) <$> replicateM (length word - 1) newState
            let t state = if state == es then []
                else let (transitionChar, _:toState:_) = fromJust . find ((==state) . head . snd) $ zip word $ windows 2 allStates
                    in if toState == es then [(ss, transitionChar), (es, transitionChar)] else [(toState, transitionChar)]
            pure $ NFA ss es (S.fromList allStates) t

acceptsString :: Eq a => NFA a Int -> [a] -> Bool
acceptsString nfa = go (S.singleton $ startState nfa)
    where
        go numInStates [] = endState nfa `S.member` numInStates
        go numInStates (c:cs) =
            let nextStates = fmap fst . concatMap (filter ((==c) . snd) . transition nfa) $ S.toList numInStates
            in go (S.fromList nextStates) cs

numAccepts :: Eq a => NFA a Int -> [a] -> Int
numAccepts nfa = go (M.singleton (startState nfa) 1)
    where
        go numInStates [] = M.findWithDefault 0 (endState nfa) numInStates
        go numInStates (c:cs) =
            let nextStates = M.unionsWith (+) .
                    fmap (\(state, count) ->
                        M.fromList . fmap ((,count) . fst) . filter ((==c) . snd) $ transition nfa state) $ M.toList numInStates
            in go nextStates cs

part1 :: Indata -> Int
part1 (ws, goals) =
    let nfa = flip evalState 0 $ mkNFA ws
    in length $ filter (acceptsString nfa) goals

part2 :: Indata -> Int
part2 (ws, goals) =
    let nfa = flip evalState 0 $ mkNFA ws
    in sum $ fmap (numAccepts nfa) goals
