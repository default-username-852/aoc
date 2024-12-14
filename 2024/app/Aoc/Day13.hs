{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Aoc.Day13(solve) where
import Linear
import Aoc.Utils (splitWhen)
import Text.Regex.TDFA ((=~))
import Data.Maybe (mapMaybe, fromJust)
import Data.LinearProgram (Direction(Min), LP (..), Constraint (Constr), Bounds (Equ, LBound), VarKind (IntVar, ContVar), GLPOpts (), mipDefaults, glpSolveVars, ReturnCode (Success))
import Data.Int (Int64)

type Indata = [(V2 Int, V2 Int, V2 Int)]

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print =<< part2 parsed

extractVector :: String -> V2 Int
extractVector l =
    let (_ :: String,_ :: String,_ :: String,x:y:_) = l =~ "Button .: X\\+([0-9]+), Y\\+([0-9]+)" in V2 (read x) (read y)

extractGoal :: String -> V2 Int
extractGoal l =
    let (_ :: String,_ :: String,_ :: String,x:y:_) = l =~ "Prize: X=([0-9]+), Y=([0-9]+)" in V2 (read x) (read y)

parseData :: String -> Indata
parseData input = fmap (\(v1:v2:goal:_) -> (extractVector v1, extractVector v2, extractGoal goal)) . splitWhen (=="") $ lines input

findSolution :: (V2 Int, V2 Int, V2 Int) -> Maybe Int
findSolution (v1, v2, goal) = let costs = [3 * a + b | a <- [0..100], b <- [0..100], v1 ^* a + v2 ^* b == goal]
    in if null costs then Nothing else Just $ minimum costs

part1 :: Indata -> Int
part1 indata = sum (mapMaybe findSolution indata)

mkLinProg :: (V2 Int, V2 Int, V2 Int) -> LP Int Int
mkLinProg (V2 ax ay, V2 bx by, V2 goalx goaly) = LP {
    direction = Min,
    objective = [(0,3), (1,1)],
    constraints = [
        Constr Nothing [(0,fromIntegral ax),(1,fromIntegral bx)] (Equ $ fromIntegral goalx),
        Constr Nothing [(0,fromIntegral ay),(1,fromIntegral by)] (Equ $ fromIntegral goaly)
    ],
    varBounds = [
        (0, LBound 0),
        (1, LBound 0)
    ],
    varTypes = [
        (0, IntVar),
        (1, IntVar)
    ]
}

linProgOpts :: GLPOpts
linProgOpts = mipDefaults

part2 :: Indata -> IO Double
part2 indata = do
    let linprogs = mkLinProg . fmap (+10000000000000) <$> indata
    solved <- mapM (glpSolveVars linProgOpts) linprogs
    pure . sum . fmap (fst . fromJust . snd) $ filter ((==Success) . fst) solved
