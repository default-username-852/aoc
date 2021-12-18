import Utils
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Either
import Data.Function

type Indata = String

main :: IO ()
main = do
    indata <- parseData <$> readFile "16.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData = (>>=hexToBinary)

hexToBinary :: Char -> String
hexToBinary '0' = "0000"
hexToBinary '1' = "0001"
hexToBinary '2' = "0010"
hexToBinary '3' = "0011"
hexToBinary '4' = "0100"
hexToBinary '5' = "0101"
hexToBinary '6' = "0110"
hexToBinary '7' = "0111"
hexToBinary '8' = "1000"
hexToBinary '9' = "1001"
hexToBinary 'A' = "1010"
hexToBinary 'B' = "1011"
hexToBinary 'C' = "1100"
hexToBinary 'D' = "1101"
hexToBinary 'E' = "1110"
hexToBinary 'F' = "1111"

parseBinary :: String -> Int
parseBinary = foldl (\acc e -> if e == '0' then 2*acc else 2*acc+1) 0

data Packet = Packet Int Int PacketType deriving Show
data PacketType = Leaf Int | Tree [Packet] deriving Show
data LengthMode = Bits Int | Packets Int

header :: Parser (Int, Int)
header = do
    version <- fmap parseBinary . count 3 $ anyChar
    t <- fmap parseBinary . count 3 $ anyChar
    return (version, t)

packet :: Parser Packet
packet = do
    (v, t) <- header
    fmap (Packet v t) $ if t == 4
        then leaf
        else tree

tree :: Parser PacketType
tree = do
    m <- choice [bitMode, packetMode]
    case m of
        Bits b -> fmap Tree $ packetWhileBits b
        Packets p -> fmap Tree $ count p packet
    
packetWhileBits :: Int -> Parser [Packet]
packetWhileBits n 
    | n <= 0 = return []
    | otherwise = do
        pos <- fmap sourceColumn getPosition
        p <- packet
        pos' <- fmap sourceColumn getPosition
        fmap (p:) $ packetWhileBits (n-pos'+pos)

bitMode :: Parser LengthMode
bitMode = do
    char '0'
    fmap (Bits . parseBinary) $ count 15 anyChar
    
packetMode :: Parser LengthMode
packetMode = do
    char '1'
    fmap (Packets . parseBinary) $ count 11 anyChar

leaf :: Parser PacketType
leaf = do
    g <- group
    return $ Leaf g
   
group :: Parser Int
group = do
    h <- many recGroup
    t <- termGroup
    return . parseBinary $ concat h ++ t

recGroup :: Parser String
recGroup = do
    char '1'
    count 4 anyChar

termGroup :: Parser String
termGroup = do
    char '0'
    count 4 anyChar

sumVersionNumbers :: Packet -> Int
sumVersionNumbers (Packet v _ (Leaf _)) = v
sumVersionNumbers (Packet v _ (Tree ps)) = (+v) . sum $ fmap sumVersionNumbers ps

evalPacket :: Packet -> Int
evalPacket (Packet _ _ (Leaf v)) = v
evalPacket (Packet _ 0 (Tree ps)) = sum $ fmap evalPacket ps
evalPacket (Packet _ 1 (Tree ps)) = product $ fmap evalPacket ps
evalPacket (Packet _ 2 (Tree ps)) = minimum $ fmap evalPacket ps
evalPacket (Packet _ 3 (Tree ps)) = maximum $ fmap evalPacket ps
evalPacket (Packet _ 5 (Tree (a:b:_))) = if on (>) evalPacket a b then 1 else 0
evalPacket (Packet _ 6 (Tree (a:b:_))) = if on (<) evalPacket a b then 1 else 0
evalPacket (Packet _ 7 (Tree (a:b:_))) = if on (==) evalPacket a b then 1 else 0

part1 :: Indata -> Int
part1 indata = sumVersionNumbers . fromRight undefined $ runParser packet () "" indata

part2 :: Indata -> Int
part2 indata = evalPacket . fromRight undefined $ runParser packet () "" indata
