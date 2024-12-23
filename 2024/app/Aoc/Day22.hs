{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE BangPatterns #-}
module Aoc.Day22(solve) where
import Data.Int (Int64, Int32)
import Data.Bits (Bits(..))
import Prelude hiding (round)
import qualified Data.IntMap.Strict as M
import Aoc.Utils (windows)
import Data.Maybe (fromMaybe)
import Control.Parallel.Strategies (parList, rdeepseq, withStrategy)

type Indata = [Int32]

solve :: String -> IO ()
solve indata = do
    let parsed = parseData indata
    print $ part1 parsed
    print $ part2 parsed

parseData :: String -> Indata
parseData = fmap read . lines

mix :: Int32 -> Int32 -> Int32
mix = xor

prune :: Int32 -> Int32
prune a = a `mod` 16777216

round1 :: Int32 -> Int32
round1 a = prune $ mix a $ a `shiftL` 6

round2 :: Int32 -> Int32
round2 a = prune $ mix a $ a `shiftR` 5

round3 :: Int32 -> Int32
round3 a = prune $ mix a $ a `shiftL` 11

round :: Int32 -> Int32
round = round3 . round2 . round1

part1 :: Indata -> Int64
part1 indata = sum $ fmap (\n -> fromIntegral $ iterate round n !! 2000) indata

packDigits :: Int -> Int -> Int -> Int -> Int
packDigits d1 d2 d3 d4 = (d1 + 9) `shiftL` 15 .|. (d2 + 9) `shiftL` 10 .|. (d3 + 9) `shiftL` 5 .|. (d4 + 9)

genMap :: Int32 -> M.IntMap Int
genMap n
    = M.fromList 
    . reverse
    . fmap (\((d1,_):(d2,_):(d3,_):(d4,fp):_) -> (packDigits d1 d2 d3 d4, fp)) 
    . windows 4 
    . fmap (\(a:b:_) -> (fromIntegral $ b - a, fromIntegral b)) 
    . windows 2 
    . take 2001 
    . fmap (`mod` 10)
    $ iterate round n

part2 :: Indata -> Int
part2 indata =
    let !ms = fmap genMap indata
        vals = withStrategy (parList rdeepseq) $ do
            d1 <- [-9..9]
            d2 <- [-9..9]
            d3 <- [-9..9]
            d4 <- [-9..9]
            pure . sum $ fmap (fromMaybe 0 . M.lookup (packDigits d1 d2 d3 d4)) ms
    in maximum vals
