import Utils
import Control.Applicative (liftA3)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function
import Data.Maybe
import Data.Either
import Debug.Trace
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator

type Indata = [Insn]
type Point = (Int,Int,Int)

data Cube = Cube (Int, Int) (Int,Int) (Int,Int) deriving (Show)
data Insn = Insn Cube Op deriving (Show)
data Op = Add | Remove deriving (Show)

number :: Parser Int
number = do
    sign <- optionMaybe $ char '-'
    num <- many digit
    return $ read num * (if isJust sign
        then (-1)
        else 1)

op :: Parser Op
op = choice $ fmap try [ string "on" *> pure Add
                       , string "off" *> pure Remove
                       ]

range :: Char -> Parser (Int,Int)
range c = do
    char c
    char '='
    l <- number
    string ".."
    u <- number
    return (l,u)

row :: Parser Insn
row = do
    o <- op
    space
    x <- range 'x'
    char ','
    y <- range 'y'
    char ','
    z <- range 'z'
    return $ Insn (Cube x y z) o

volume :: Cube -> Int
volume (Cube (xl,xu) (yl,yu) (zl,zu)) = (xu-xl+1)*(yu-yl+1)*(zu-zl+1)

intersects :: Cube -> Cube -> Bool
intersects (Cube (xl1,xu1) (yl1,yu1) (zl1,zu1)) (Cube (xl2,xu2) (yl2,yu2) (zl2,zu2)) =
       min xu1 xu2 >= max xl1 xl2
    && min yu1 yu2 >= max yl1 yl2
    && min zu1 zu2 >= max zl1 zl2

remove :: Cube -> Cube -> [Cube]
remove c1@(Cube (xl',xu') (yl',yu') (zl',zu')) c2@(Cube (xl,xu) (yl,yu) (zl,zu))
    | not $ intersects c1 c2 = return c2
    | otherwise = filter ((>0) . volume) 
        $ [ Cube (xl,xb-1) (yl,yu) (zl,zu)
          , Cube (xt+1,xu) (yl,yu) (zl,zu)
          , Cube (xb,xt) (yl,yb-1) (zl,zu)
          , Cube (xb,xt) (yt+1,yu) (zl,zu)
          , Cube (xb,xt) (yb,yt) (zl,zb-1)
          , Cube (xb,xt) (yb,yt) (zt+1,zu)
          ]
    where
        (xb,xt) = (max xl xl', min xu xu')
        (yb,yt) = (max yl yl', min yu yu')
        (zb,zt) = (max zl zl', min zu zu')

runInsn :: Insn -> S.Set Point -> S.Set Point
runInsn (Insn (Cube (xl,xu) (yl,yu) (zl,zu)) Add) set = set `S.union` S.fromList (liftA3 (,,) [max xl (-50)..min xu 50] [max yl (-50)..min yu 50] [max zl (-50)..min zu 50])
runInsn (Insn (Cube (xl,xu) (yl,yu) (zl,zu)) Remove) set = set `S.difference` S.fromList (liftA3 (,,) [max xl (-50)..min xu 50] [max yl (-50)..min yu 50] [max zl (-50)..min zu 50])

runInsn' :: Insn -> [Cube] -> [Cube]
runInsn' (Insn adding Add) cs = adding:(cs >>= (remove adding))
runInsn' (Insn adding Remove) cs = cs >>= (remove adding)

main :: IO ()
main = do
    indata <- parseData <$> readFile "22.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData = fromRight undefined . runParser (many1 (row <* endOfLine)) () "22.in"

part1 :: Indata -> Int
part1 indata = foldl (flip runInsn) S.empty indata & S.toList & filter (\(x,y,z) -> x >= -50 && x <= 50 && y >= -50 && y <= 50 && z >= -50 && z <= 50) & length

part2 :: Indata -> Int
part2 indata = foldl (flip runInsn') [] indata & fmap volume & sum
