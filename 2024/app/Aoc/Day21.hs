module Aoc.Day21(solve) where
import Linear (V2(..))
import Control.Monad (guard)
import Data.List (scanl')
import qualified Data.Map as M
import Data.Foldable (Foldable(..))

type Indata = [String]

data Press = UpPress | DownPress | LeftPress | RightPress | APress deriving (Show, Ord, Eq)

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part 2 parsed
    print $ part 25 parsed

parseData :: String -> Indata
parseData = lines

keypadLetterPos :: Char -> V2 Int
keypadLetterPos '0' = V2 1 3
keypadLetterPos '1' = V2 0 2
keypadLetterPos '2' = V2 1 2
keypadLetterPos '3' = V2 2 2
keypadLetterPos '4' = V2 0 1
keypadLetterPos '5' = V2 1 1
keypadLetterPos '6' = V2 2 1
keypadLetterPos '7' = V2 0 0
keypadLetterPos '8' = V2 1 0
keypadLetterPos '9' = V2 2 0
keypadLetterPos 'A' = V2 2 3
keypadLetterPos _ = error "unknown letter"

v2ToPress :: V2 Int -> Press
v2ToPress (V2 0 1) = DownPress
v2ToPress (V2 1 0) = RightPress
v2ToPress (V2 (-1) 0) = LeftPress
v2ToPress (V2 0 (-1)) = UpPress
v2ToPress v = error $ "invalid vector: " <> show v

pressesForKeypad :: V2 Int -> [Char] -> [[Press]]
pressesForKeypad _ [] = [[]]
pressesForKeypad at@(V2 atX atY) (c:cs) =
    let newPos@(V2 newX newY) = keypadLetterPos c
        dx = abs $ newX - atX
        dy = abs $ newY - atY
        xMove = V2 (signum $ newX - atX) 0
        yMove = V2 0 (signum $ newY - atY)
        xMoves = replicate dx xMove
        yMoves = replicate dy yMove
        inBounds :: V2 Int -> Bool
        inBounds p = p /= V2 0 3
    in do
        moves <- mergeLists xMoves yMoves
        guard $ all inBounds $ scanl' (+) at moves
        nextPresses <- pressesForKeypad newPos cs
        pure $ fmap v2ToPress moves <> [APress] <> nextPresses

movePadKeyPos :: Press -> V2 Int
movePadKeyPos UpPress = V2 1 0
movePadKeyPos DownPress = V2 1 1
movePadKeyPos LeftPress = V2 0 1
movePadKeyPos RightPress = V2 2 1
movePadKeyPos APress = V2 2 0

pressesForMovePad :: V2 Int -> [Press] -> [[Press]]
pressesForMovePad _ [] = [[]]
pressesForMovePad at@(V2 atX atY) (c:cs) =
    let newPos@(V2 newX newY) = movePadKeyPos c
        dx = abs $ newX - atX
        dy = abs $ newY - atY
        xMove = V2 (signum $ newX - atX) 0
        yMove = V2 0 (signum $ newY - atY)
        xMoves = replicate dx xMove
        yMoves = replicate dy yMove
        inBounds :: V2 Int -> Bool
        inBounds p = p /= V2 0 0
    in do
        moves <- mergeLists xMoves yMoves
        guard $ all inBounds $ scanl' (+) at moves
        nextPresses <- pressesForMovePad newPos cs
        pure $ fmap v2ToPress moves <> [APress] <> nextPresses

mergeLists :: [a] -> [a] -> [[a]]
mergeLists [] bs = [bs]
mergeLists as [] = [as]
mergeLists (a:as) (b:bs) = fmap (a:) (mergeLists as (b:bs)) <> fmap (b:) (mergeLists (a:as) bs)

countMoves :: M.Map (V2 Int, Press) Int -> [Press] -> Int
countMoves prevCosts =
    fst . foldl' (\(cost, prevPos) move ->
        (cost + (prevCosts M.! (prevPos, move)), movePadKeyPos move)) (0, movePadKeyPos APress)

evalAllRobotMovesCosts :: M.Map (V2 Int, Press) Int -> M.Map (V2 Int, Press) Int
evalAllRobotMovesCosts prevCosts =
    let allPresses = [LeftPress, RightPress, UpPress, DownPress, APress]
    in M.fromList $ do
        startPos <- fmap movePadKeyPos allPresses
        toPress <- allPresses
        let abovePresses = pressesForMovePad startPos [toPress]
        pure ((startPos, toPress), minimum $ fmap (countMoves prevCosts) abovePresses)

firstRobotMoveCosts :: M.Map (V2 Int, Press) Int
firstRobotMoveCosts = M.fromList $ do
    startPos <- fmap movePadKeyPos allPresses
    toPress <- allPresses
    let abovePresses = pressesForMovePad startPos [toPress]
    pure ((startPos, toPress), minimum $ fmap length abovePresses)
    where
        allPresses = [LeftPress, RightPress, UpPress, DownPress, APress]

part :: Int -> Indata -> Int
part iters indata =
    let costs = iterate evalAllRobotMovesCosts firstRobotMoveCosts !! (iters - 1)
        scoreInput :: String -> Int
        scoreInput s =
            let moves = minimum . fmap (countMoves costs) . pressesForKeypad (keypadLetterPos 'A') $ s
                multiplier = read $ init s
            in moves * multiplier
    in sum $ fmap scoreInput indata
