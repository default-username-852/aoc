import Options.Applicative
import qualified Aoc.Day1
import qualified Aoc.Day2
import qualified Aoc.Day3
import qualified Aoc.Day4
import qualified Aoc.Day5
import qualified Aoc.Day6
import qualified Aoc.Day7
import qualified Aoc.Day8
import qualified Aoc.Day9
import qualified Aoc.Day10
import qualified Aoc.Day11
import qualified Aoc.Day12
import qualified Aoc.Day13
import qualified Aoc.Day14
import qualified Aoc.Day15
import qualified Aoc.Day16
import qualified Aoc.Day17
import qualified Aoc.Day18
import qualified Aoc.Day19
import qualified Aoc.Day20
import qualified Aoc.Day21
import qualified Aoc.Day22
import qualified Aoc.Day23
import qualified Aoc.Day24

args :: Parser Int
args = option auto (long "day" <> short 'd')

days :: [String -> IO ()]
days =
    [ Aoc.Day1.solve
    , Aoc.Day2.solve
    , Aoc.Day3.solve
    , Aoc.Day4.solve
    , Aoc.Day5.solve
    , Aoc.Day6.solve
    , Aoc.Day7.solve
    , Aoc.Day8.solve
    , Aoc.Day9.solve
    , Aoc.Day10.solve
    , Aoc.Day11.solve
    , Aoc.Day12.solve
    , Aoc.Day13.solve
    , Aoc.Day14.solve
    , Aoc.Day15.solve
    , Aoc.Day16.solve
    , Aoc.Day17.solve
    , Aoc.Day18.solve
    , Aoc.Day19.solve
    , Aoc.Day20.solve
    , Aoc.Day21.solve
    , Aoc.Day22.solve
    , Aoc.Day23.solve
    , Aoc.Day24.solve
    ]

main :: IO ()
main = do
    day <- execParser (info args mempty)
    getContents >>= days !! (day - 1)
