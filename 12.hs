import Utils

data Instruction = N (Vec2 -> Vec2) | S (Vec2 -> Vec2) | E (Vec2 -> Vec2) | W (Vec2 -> Vec2) | L (Vec2 -> Vec2) | R (Vec2 -> Vec2) | F (Vec2 -> Vec2 -> Vec2)

main :: IO ()
main = do
    indata <- readFile "12.in"
    print $ part1 indata
    print $ part2 indata

part1 :: String -> Int
part1 indata = let (Vec2 x y, _) = foldl runInstruction (Vec2 0 0, Vec2 1 0) $ parseData indata in abs x + abs y

parseData :: String -> [Instruction]
parseData = map parseInstruction . lines

parseInstruction :: String -> Instruction
parseInstruction ('N':num) = N $ addVec $ Vec2 0   (read num)
parseInstruction ('S':num) = S $ addVec $ Vec2 0 (-(read num))
parseInstruction ('E':num) = E $ addVec $ Vec2   (read num)  0
parseInstruction ('W':num) = W $ addVec $ Vec2 (-(read num)) 0
parseInstruction ('L':num) = L $ last $ take (read num `div` 90) $ tail $ iterate ((matMul $ Mat2 (Vec2 0 1) (Vec2 (-1) 0)) .) id
parseInstruction ('R':num) = R $ last $ take (read num `div` 90) $ tail $ iterate ((matMul $ Mat2 (Vec2 0 (-1)) (Vec2 1 0)) .) id
parseInstruction ('F':num) = F $ \dir -> last $ take (read num) $ tail $ iterate (addVec dir .) id

runInstruction :: (Vec2, Vec2) -> Instruction -> (Vec2, Vec2)
runInstruction (pos, dir) (N func) = (func pos, dir)
runInstruction (pos, dir) (S func) = (func pos, dir)
runInstruction (pos, dir) (E func) = (func pos, dir)
runInstruction (pos, dir) (W func) = (func pos, dir)
runInstruction (pos, dir) (L func) = (pos, func dir)
runInstruction (pos, dir) (R func) = (pos, func dir)
runInstruction (pos, dir) (F func) = (func dir pos, dir)

part2 :: String -> Int
part2 indata = let (Vec2 x y, _) = foldl runInstruction' (Vec2 0 0, Vec2 10 1) $ parseData indata in abs x + abs y

runInstruction' :: (Vec2, Vec2) -> Instruction -> (Vec2, Vec2)
runInstruction' (pos, dir) (N func) = (pos, func dir)
runInstruction' (pos, dir) (S func) = (pos, func dir)
runInstruction' (pos, dir) (E func) = (pos, func dir)
runInstruction' (pos, dir) (W func) = (pos, func dir)
runInstruction' (pos, dir) (L func) = (pos, func dir)
runInstruction' (pos, dir) (R func) = (pos, func dir)
runInstruction' (pos, dir) (F func) = (func dir pos, dir)

data Vec2 = Vec2 Int Int deriving (Show, Eq)
data Mat2 = Mat2 Vec2 Vec2 deriving (Show, Eq)

addVec :: Vec2 -> Vec2 -> Vec2
addVec (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)

matMul :: Mat2 -> Vec2 -> Vec2
matMul (Mat2 (Vec2 a c) (Vec2 b d)) (Vec2 x y) = Vec2 (a*x + b*y) (c*x + d*y)
