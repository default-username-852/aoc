import Utils
import Control.Monad
import Control.Monad.State.Lazy
import Data.Functor.Identity
import Data.Function
import Data.Monoid
import Data.MemoCombinators.Class

type Indata = (Int, Int)
type Die = ([Int], Int)

data Game = Game { getP1Pos, getP2Pos, p1Score, p2Score :: Int } deriving (Show)

p1pos, p2pos :: Int
p1pos = 1
p2pos = 6

die :: Die
die = (cycle [1..100], 0)

rollDie :: StateT Die Identity Int
rollDie = state $ \((x:xs), n) -> (x,(xs,n+1))

playP1 :: StateT Game (StateT Die Identity) ()
playP1 = do
    d1 <- lift rollDie
    d2 <- lift rollDie
    d3 <- lift rollDie
    modify (\(g@Game{getP1Pos = p}) -> g {getP1Pos = ((p+d1+d2+d3-1) `mod` 10)+1})
    modify (\(g@Game{getP1Pos = p, p1Score = s}) -> g {p1Score = p + s})
    return ()

playP2 :: StateT Game (StateT Die Identity) ()
playP2 = do
    d1 <- lift rollDie
    d2 <- lift rollDie
    d3 <- lift rollDie
    modify (\(g@Game{getP2Pos = p}) -> g {getP2Pos = ((p+d1+d2+d3-1) `mod` 10)+1})
    modify (\(g@Game{getP2Pos = p, p2Score = s}) -> g {p2Score = p + s})
    return ()

gameOver :: StateT Game (StateT Die Identity) Bool
gameOver = gets (\(Game{ p1Score = s1, p2Score = s2 }) -> s1 >= 1000 || s2 >= 1000)

playGame :: StateT Game (StateT Die Identity) Int
playGame = do
    playP1
    w <- gameOver
    if w 
        then do
            s <- gets p2Score
            d <- lift $ gets snd
            return $ s * d
        else do
            playP2
            w2 <- gameOver
            if w2
                then do
                    s <- gets p1Score
                    d <- lift $ gets snd
                    return $ s * d
                else playGame

mod10 :: Int -> Int
mod10 n = (n-1) `mod` 10 & (+1)

noWins :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
noWins = memoize noWins'
    where
        noWins' :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
        noWins' p1 p2 s1 s2 t
            | s1 >= 21 = (1,0)
            | s2 >= 21 = (0,1)
            | t == 0 = zip [3..9] [1,3,6,7,6,3,1] & fmap (\(offset,ways) -> let (a,b) = noWins (mod10 $ p1 + offset) p2 (s1 + (mod10 $ p1 + offset)) s2 1 in (ways * a, ways * b)) & foldl (\(a,b) (x,y) -> (a+x,b+y)) (0,0)
            | t == 1 = zip [3..9] [1,3,6,7,6,3,1] & fmap (\(offset,ways) -> let (a,b) = noWins p1 (mod10 $ p2 + offset) s1 (s2 + (mod10 $ p2 + offset)) 0 in (ways * a, ways * b)) & foldl (\(a,b) (x,y) -> (a+x,b+y)) (0,0)

main :: IO ()
main = do
    print $ part1 (p1pos, p2pos)
    print $ part2 (p1pos, p2pos)

part1 :: Indata -> Int
part1 (p1p, p2p) = evalStateT playGame (Game {getP1Pos = p1p, getP2Pos = p2p, p1Score = 0, p2Score = 0}) & flip evalStateT die & runIdentity

part2 :: Indata -> Int
part2 (p1, p2) = let (a,b) = noWins p1 p2 0 0 0 in max a b
