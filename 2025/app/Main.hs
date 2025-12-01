import Options.Applicative
import qualified Aoc.Day1

args :: Parser Int
args = option auto (long "day" <> short 'd')

days :: [String -> IO ()]
days =
    [ Aoc.Day1.solve
    ]

main :: IO ()
main = do
    day <- execParser (info args mempty)
    getContents >>= days !! (day - 1)
