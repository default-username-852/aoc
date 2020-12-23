import Utils
import Data.List
import Debug.Trace
import qualified Data.Sequence as S
import Data.Sequence (Seq ((:<|)), Seq((:|>)), elemIndexL, insertAt, (|>), (!?))
import Data.Maybe

indata :: [Int]
indata = map (read . (:[])) "476138259"

main :: IO ()
main = print (part1 indata) >> print (part2 indata)

part1 :: [Int] -> S.Seq Int
part1 indata = (!! 100) $ iterate doMove' $ S.fromList indata

part2 :: [Int] -> Int
part2 indata = fromJust (found !? (oneIdx + 1)) * fromJust (found !? (oneIdx + 2))
    where
        found = (!! 10000000) $ iterate doMove' $ S.fromList $ take 1000000 $ indata ++ [length indata + 1..]
        Just oneIdx = elemIndexL 1 found

doMove :: [Int] -> [Int]
doMove all@(current:p1:p2:p3:rest) = let (some, found:others) = break (==finding) rest in some ++ [found] ++ [p1, p2, p3] ++ others ++ [current]
    where
        Just finding = find (not . flip elem [p1, p2, p3]) $ [current - 1, current - 2 .. 1] ++ [length all, length all - 1 .. current]

doMove' :: S.Seq Int -> S.Seq Int
doMove' all@(current:<|p1:<|p2:<|p3:<|rest) = let Just idx = elemIndexL finding rest in inserter (idx + 1) rest |> current
    where
        Just finding = find (not . flip elem [p1, p2, p3]) $ [current - 1, current - 2 .. 1] ++ [length all, length all - 1 .. current]
        inserter idx = insertAt idx p1 . insertAt idx p2 . insertAt idx p3
