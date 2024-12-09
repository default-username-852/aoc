module Aoc.Day8(solve) where
import qualified Data.Map.Strict as M
import Linear
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad
import qualified Data.Set as S
import Control.Applicative (Applicative (liftA2))

data Indata = Indata {
    mapUpperBounds :: V2 Int,
    mapAntennas :: M.Map Char (NonEmpty (V2 Int))
} deriving Show

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

parseData :: String -> Indata
parseData input =
    let annotated = (concatMap (\(y, row) -> (\(x, c) -> (V2 (x :: Int) y, c)) <$> zip [0..] row) . zip [0..]) $ lines input
        antennas = foldr (\(pos, frequency) accum -> M.insertWith (<>) frequency (pure pos) accum) M.empty $ filter ((/='.') . snd) annotated
        upperBound = maximum $ fmap fst annotated
    in Indata { mapAntennas = antennas, mapUpperBounds = upperBound }

antinodes :: NonEmpty (V2 Int) -> [V2 Int]
antinodes antennas = do
    a1 <- NE.toList antennas
    a2 <- NE.toList antennas
    guard $ a1 > a2
    let diff = a1 - a2
    [a1 + diff, a2 - diff]

part1 :: Indata -> Int
part1 indata =
    S.size .
    S.filter (\e -> not $ any (<0) e || any (<0) (liftA2 (-) (mapUpperBounds indata) e)) .
    mconcat . fmap (S.fromList . antinodes) .
    M.elems $ mapAntennas indata

allAntinodes :: V2 Int -> NonEmpty (V2 Int) -> [V2 Int]
allAntinodes upperBound@(V2 maxX maxY) antennas = do
    a1 <- NE.toList antennas
    a2 <- NE.toList antennas
    guard $ a1 > a2
    let V2 dx dy = a1 - a2
    let divider = gcd dx dy
    filter (\e -> all (>=0) e && all (>=0) (liftA2 (-) upperBound e)) $
        fmap (\delta -> a1 + delta *^ V2 (dx `div` divider) (dy `div` divider)) [(-(max maxX maxY))*2..max maxX maxY*2]

part2 :: Indata -> Int
part2 indata =
    S.size .
    mconcat . fmap (S.fromList . allAntinodes (mapUpperBounds indata)) .
    M.elems $ mapAntennas indata
