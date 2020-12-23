import Utils
import Data.List
import qualified Data.Set as S

main :: IO ()
main = do
    indata <- readFile "22.in"
    print $ part1 indata
    print $ part2 indata

part1 :: String -> Int
part1 indata = sum $ zipWith (*) [length winner, length winner - 1 ..] winner
    where
        Just (a, b) = find (\(a, b) -> null a || null b) $ iterate playRound $ parseData indata
        winner = if null a then b else a

part2 :: String -> Int
part2 indata = sum $ zipWith (*) [length winner, length winner - 1 ..] winner
    where 
        winner = snd $ playGame $ parseData indata

parseData :: String -> ([Int], [Int])
parseData indata = let (p1:p2:_) = splitWhen (=="") $ lines indata in (map read $ tail p1, map read $ tail p2)

playRound :: ([Int], [Int]) -> ([Int], [Int])
playRound ((p1:rest1), (p2:rest2))
    | p1 > p2 = (rest1 ++ [p1, p2], rest2)
    | p1 < p2 = (rest1, rest2 ++ [p2, p1])

playRound' :: ([Int], [Int], S.Set ([Int], [Int])) -> ([Int], [Int], S.Set ([Int], [Int]))
playRound' (p1@(a:rest1), p2@(b:rest2), seen)
    | S.member (p1, p2) seen = (p1 ++ p2, [], newSeen)
    | length rest1 >= a && length rest2 >= b = let (weiner, _) = playGame (take a rest1, take b rest2) in fromWinner weiner
    | otherwise = fromWinner (a > b)
    where
        newSeen = S.insert (p1, p2) seen
        fromWinner winner = if winner then (rest1 ++ [a, b], rest2, newSeen) else (rest1, rest2 ++ [b, a], newSeen)

-- True for p1
playGame :: ([Int], [Int]) -> (Bool, [Int])
playGame (p1, p2) = (null b, winner)
    where
        Just (a, b, _) = find (\(a, b, _) -> null a || null b) $ iterate playRound' (p1, p2, S.empty)
        winner = if null a then b else a
