{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Aoc.Day24(solve) where
import qualified Data.Map as M
import Data.List (sortBy)
import qualified Data.Ord
import Data.Foldable (Foldable(foldl'), find)
import Data.Int (Int64)
import Text.Printf (printf)
import Data.Bits (Bits(testBit))
import Data.Ord (comparing)
import Data.Maybe (isJust)
import Control.Monad (guard)
import Debug.Trace (traceShowId)

type Indata = (M.Map String Bool, [(String, String, String, String)])

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

funcName :: String -> Bool -> Bool -> Bool
funcName "AND" = (&&)
funcName "OR" = (||)
funcName "XOR" = (/=)
funcName _ = error "unknown function"

parseData :: String -> Indata
parseData input =
    let (initialValsLines, _:restLines) = break (=="") $ lines input
        initialVals = M.fromList $ fmap ((\(var:val:_) -> (init var, read val == (1 :: Int))) . words) initialValsLines
        rests = fmap ((\(a:f:b:_:to:_) -> (a, f, b, to)) . words) restLines
    in (initialVals, rests)

evalCircuit :: M.Map String Bool -> [(String, String, String, String)] -> M.Map String Bool
evalCircuit initialVals otherVals =
    let fullVals = initialVals `M.union` M.fromList (fmap (\(a, f, b, res) -> (res, funcName f (fullVals M.! a) (fullVals M.! b))) otherVals)
    in fullVals

part1 :: Indata -> Int64
part1 indata =
    let fullMap = uncurry evalCircuit indata
    in readAnswer fullMap

numsToInitialMap :: Int64 -> Int64 -> M.Map String Bool
numsToInitialMap x y =
    let xVars = [(printf "x%02d" i, x `testBit` i) | i <- [0..44]]
        yVars = [(printf "y%02d" i, y `testBit` i) | i <- [0..44]]
    in M.fromList $ xVars <> yVars

readAnswer :: M.Map String Bool -> Int64
readAnswer m =
    let keyVars = (sortBy (comparing Data.Ord.Down) . filter ('z' `elem`)) $ M.keys m
    in foldl' (\acc v -> if v then acc*2 + 1 else acc*2) 0 $ fmap (m M.!) keyVars

findWiresInAdder :: Int -> [(String, String, String, String)] -> Maybe [String]
findWiresInAdder bit ckt = do
    let xIn = printf "x%02d" bit
    let yIn = printf "y%02d" bit
    (_,_,_,xor1) <- find (\(a, func, b, _) -> (a == xIn || b == xIn) && (b == yIn || a == yIn) && func == "XOR") ckt
    (_,_,_,and1) <- find (\(a, func, b, _) -> (a == xIn || b == xIn) && (b == yIn || a == yIn) && func == "AND") ckt
    (cand1,_,cand2,xor2) <- find (\(a, func, b, _) -> (a == xor1 || b == xor1) && func == "XOR") ckt
    let cin = if cand1 == xor1 then cand2 else cand1
    (_,_,_,and2) <- find (\(a, func, b, _) -> (a == cin || b == cin) && (a == xor1 || b == xor1) && func == "AND") ckt
    (_,_,_,or1) <- find (\(a, func, b, _) -> (a == and1 || b == and1) && (a == and2 || b == and2) && func == "OR") ckt
    guard $ xor2 == printf "z%02d" bit
    pure [cin, xor1, and1, xor2, and2, or1]

isAdder :: Int -> [(String, String, String, String)] -> Bool
isAdder bit ckt = isJust $ findWiresInAdder bit ckt

part2 :: Indata -> Int
part2 indata = 0
