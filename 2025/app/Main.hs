import Options.Applicative
import qualified Aoc.Day1
import qualified Aoc.Day2

args :: Parser Int
args = option auto (long "day" <> short 'd')

days :: [String -> IO ()]
days =
    [ Aoc.Day1.solve
    , Aoc.Day2.solve
    ]

main :: IO ()
main = do
    day <- execParser (info args mempty)
    getContents >>= days !! (day - 1)
