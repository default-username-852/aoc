module Aoc.Day9(solve) where
import Data.Array (Array, listArray, (!), (//), bounds, assocs, elems, Ix (..))
import Data.Maybe (isJust, fromJust)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Foldable (find)
import Aoc.Utils (windows)

data File = File Integer Int | Empty Int deriving Show

type Indata = [File]

solve :: String -> IO ()
solve indata = do
    let parsed = parseData 0 indata
    print $ part1 parsed
    print $ part2 parsed

parseData :: Integer -> String -> Indata
parseData _ [] = []
parseData nextId [full] = [File nextId . read $ [full]]
parseData nextId (full:empty:rest) = File nextId (read [full]) : Empty (read [empty]) : parseData (nextId + 1) rest

mkMem :: Indata -> Array Int (Maybe Integer)
mkMem indata =
    let expanded = concatMap expand indata
    in listArray (0, length expanded - 1) expanded
    where
        expand :: File -> [Maybe Integer]
        expand (File id size) = replicate size $ Just id
        expand (Empty size) = replicate size Nothing

indexSwap :: Int -> Int -> Array Int (Maybe Integer) -> Array Int (Maybe Integer)
indexSwap lower upper arr
    | lower == upper = arr
    | otherwise = case arr ! lower of
        (Just _) -> indexSwap (lower + 1) upper arr
        _ -> case arr ! upper of
            Nothing -> indexSwap lower (upper - 1) arr
            _ -> indexSwap (lower + 1) (upper - 1) (arr // [(lower, arr ! upper), (upper, arr ! lower)])

reshuffleFiles :: Array Int (Maybe Integer) -> Array Int (Maybe Integer)
reshuffleFiles initial = let (lower, upper) = bounds initial in indexSwap lower upper initial

part1 :: Indata -> Integer
part1 indata = memChecksum . reshuffleFiles $ mkMem indata

moveFile :: Integer -> Array Int (Maybe Integer) -> Array Int (Maybe Integer)
moveFile fileId arr =
    let fileSize = length . filter (== Just fileId) $ elems arr
        firstFileSpot = head . fromJust . find (all ((== Just fileId) . (arr !))) . windows fileSize . range $ bounds arr
        firstFreeSpot = fmap head . find (all ((== Nothing) . (arr !))) . windows fileSize . range $ bounds arr
    in case firstFreeSpot of
        Nothing -> arr
        Just spot ->
            if spot > firstFileSpot then arr
            else arr // ([(pos + spot, arr ! (pos + firstFileSpot)) | pos <- [0..fileSize - 1]] ++
                [(pos + firstFileSpot, arr ! (pos + spot)) | pos <- [0..fileSize - 1]])

memChecksum :: Array Int (Maybe Integer) -> Integer
memChecksum = sum . fmap (uncurry (*) . bimap fromIntegral fromJust) . filter (isJust . snd) . assocs

part2 :: Indata -> Integer
part2 indata =
    let maxFileId = fromIntegral $ length indata `div` 2 -- fulhack
    in memChecksum $ foldr moveFile (mkMem indata) [0..maxFileId]
