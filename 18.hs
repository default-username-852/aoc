import Utils
import Data.Char
import Debug.Trace
import Data.Maybe

data Token = Number Integer | Plus | Star | LeftParen | RightParen deriving (Show, Eq)

data AST = ASTNum Integer | ASTPlus | ASTStar | ASTLeftParen | ASTRightParen | ASTAdd AST AST | ASTMul AST AST deriving (Show, Eq)

data Expression = Num Integer | Addition Expression Expression | Multiplication Expression Expression deriving (Show, Eq)

main :: IO ()
main = do
    indata <- readFile "18.in"
    print $ part1 indata
    print $ part2 indata

part1 :: String -> Integer
part1 indata = sum $ map (evalExpr . parseExpr . map parenFlipper . reverse . tokenize) $ map (filter (/=' ')) $ lines indata

part2 :: String -> Integer
part2 indata = sum $ map (evalExpr . astExpr . parseAST . toAST . tokenize) $ map (filter (/=' ')) $ lines indata

parenFlipper :: Token -> Token
parenFlipper LeftParen = RightParen
parenFlipper RightParen = LeftParen
parenFlipper t = t

tokenize :: String -> [Token]
tokenize "" = []
tokenize ('+':t) = Plus:tokenize t
tokenize ('*':t) = Star:tokenize t
tokenize ('(':t) = LeftParen:tokenize t
tokenize (')':t) = RightParen:tokenize t
tokenize str = let (num, t) = span isDigit str in Number (read num):tokenize t

toAST :: [Token] -> [AST]
toAST = map astizeToken

parseAST :: [AST] -> AST
parseAST = astMul . astPlus . astParens

astizeToken :: Token -> AST
astizeToken (Number n) = ASTNum n
astizeToken Plus = ASTPlus
astizeToken Star = ASTStar
astizeToken LeftParen = ASTLeftParen
astizeToken RightParen = ASTRightParen

astParens :: [AST] -> [AST]
astParens [] = []
astParens (ASTLeftParen:tokens) = let (paren, rest) = span ((>0) . snd) $ scanl depthAcc' (ASTLeftParen, 1) tokens in (parseAST $ init $ tail $ map fst paren):astParens (map fst rest)
astParens (h:t) = h:astParens t

depthAcc' :: (AST, Int) -> AST -> (AST, Int)
depthAcc' (_, depth) ASTLeftParen = (ASTLeftParen, depth + 1)
depthAcc' (ASTRightParen, depth) token = (token, depth - 1)
depthAcc' (_, depth) token = (token, depth)

astPlus :: [AST] -> [AST]
astPlus [] = []
astPlus (expr:ASTStar:rest) = expr:ASTStar:astPlus rest
astPlus [expr] = [expr]
astPlus tokens = case span astPlusType tokens of
    (unit, []) -> [astPlusExpr unit]
    (unit, rest) -> astPlusExpr unit:head rest:astPlus (tail rest)

astPlusType :: AST -> Bool
astPlusType ASTStar = False
astPlusType _ = True

astPlusExpr :: [AST] -> AST
astPlusExpr (expr:ASTPlus:rest) = ASTAdd expr (astPlusExpr rest)
astPlusExpr [expr] = expr

astMul :: [AST] -> AST
astMul (expr:ASTStar:rest) = ASTMul expr (astMul rest)
astMul [expr] = expr
astMul rest = error $ show rest

astExpr :: AST -> Expression
astExpr (ASTAdd a b) = Addition (astExpr a) (astExpr b)
astExpr (ASTMul a b) = Multiplication (astExpr a) (astExpr b)
astExpr (ASTNum n) = Num n

allSplits :: [a] -> [([a],[a])]
allSplits lst = map (flip splitAt lst) [0..length lst]

parseExpr :: [Token] -> Expression
parseExpr tokens = case breakEvalUnit tokens of
    ([Number n], []) -> Num n
    (unit, []) -> parseExpr unit
    (unit, Plus:rest) -> Addition (parseExpr unit) (parseExpr rest)
    (unit, Star:rest) -> Multiplication (parseExpr unit) (parseExpr rest)

depthAcc :: (Token, Int) -> Token -> (Token, Int)
depthAcc (_, depth) LeftParen = (LeftParen, depth + 1)
depthAcc (RightParen, depth) token = (token, depth - 1)
depthAcc (_, depth) token = (token, depth)

breakEvalUnit :: [Token] -> ([Token], [Token])
breakEvalUnit ((Number n):t) = ([Number n], t)
breakEvalUnit (LeftParen:rest) = let (paren,rest') = span ((>0) . snd) $ scanl depthAcc (LeftParen, 1) rest in (init $ tail $ map fst paren, map fst rest')

evalExpr :: Expression -> Integer
evalExpr (Num n) = n
evalExpr (Addition a b) = evalExpr a + evalExpr b
evalExpr (Multiplication a b) = evalExpr a * evalExpr b

-- <expression> ::= <number> | <expression> + <expression> | <expression> * <expression> | (<expression>)
