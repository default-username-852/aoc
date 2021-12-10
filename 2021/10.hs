import Utils
import Data.Maybe
import Data.List
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.String

type Indata = [String]

main :: IO ()
main = do
    indata <- parseData <$> readFile "10.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData = lines

chunk :: Parsec String () ()
chunk = pure () <* many (choice $
    [ between (char '{') (char '}') chunk
    , between (char '[') (char ']') chunk
    , between (char '(') (char ')') chunk
    , between (char '<') (char '>') chunk
    ])

scoreChar :: Char -> Int
scoreChar ')' = 3
scoreChar ']' = 57
scoreChar '}' = 1197
scoreChar '>' = 25137

part1 :: Indata -> Int
part1 indata = sum $ fmap scoreLine indata
    where
        scoreLine :: String -> Int
        scoreLine l = case runParser chunk () "" l of
            Right a -> 0
            Left e -> case messageString . head $ errorMessages e of
                "" -> 0
                s -> scoreChar $ s !! 1

scoreChar' :: Char -> Int
scoreChar' ')' = 1
scoreChar' ']' = 2
scoreChar' '}' = 3
scoreChar' '>' = 4

incompleteLine :: String -> Bool
incompleteLine l = case runParser chunk () "" l of
    Right _ -> True
    Left e -> (=="") . messageString . head $ errorMessages e

completeLine :: String -> String
completeLine l = fromJust . last . takeWhile (isJust) . iterate (\p -> p >>= (\p' -> fmap ((p'++) . (:[])) $ addToCompletion (l++p'))) $ Just ""

addToCompletion :: String -> Maybe Char
addToCompletion l = case runParser chunk () "" l of
    Right _ -> Nothing
    Left e -> Just . (!! 1) . messageString . last $ errorMessages e

scorePostfix :: String -> Int
scorePostfix l = foldl (\acc e -> 5*acc + scoreChar' e) 0 l

part2 :: Indata -> Int
part2 indata = let scores = sort . fmap (scorePostfix . completeLine) $ filter incompleteLine indata in scores !! (length scores `div` 2)
