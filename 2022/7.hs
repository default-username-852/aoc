import Utils
import Data.List

type Indata = [Command]

data File = File {fileSize :: Int, fileName :: String} | Dir {dirName :: String, children :: [File]} deriving Show

data Command = Cd {cdPath :: String} | Ls {lsRes :: [LsResult]} deriving Show

data LsResult = LsDir {lsDirName :: String} | LsFile {lsFileSize :: Int, lsFileName :: String} deriving Show

main :: IO ()
main = do
    indata <- parseData <$> readFile "7.in"
    print $ part1 indata
    print $ part2 indata

parseData :: String -> Indata
parseData = parseCommands . lines

parseCommands :: [String] -> [Command]
parseCommands [] = []
parseCommands (('$':' ':'c':'d':' ':path):t) = (Cd {cdPath = path}):parseCommands t
parseCommands (('$':' ':'l':'s':_):t) = (Ls {lsRes = fmap parseLineLs commandOutput}):parseCommands rest
    where
        (commandOutput, rest) = break ((=='$') . head) t
        parseLineLs :: String -> LsResult
        parseLineLs ('d':'i':'r':' ':dirName) = LsDir {lsDirName = dirName}
        parseLineLs l = let (size:name:_) = words l in LsFile {lsFileSize = read size, lsFileName = name}

type ParsingState = ([String], File)

updateTree :: [String] -> File -> [LsResult] -> File
updateTree [] d@(Dir {}) newChildren = d {children = fmap fileifyLs newChildren}
updateTree (p:t) d@(Dir {children = c}) newChildren = d {children = modifyAt (updateTree t found newChildren) idx c}
    where
        (Just (idx, found)) = find (matchesName . snd) $ zip [0..] c
        matchesName :: File -> Bool
        matchesName (File {}) = False
        matchesName (Dir {dirName = name}) = name == p

interpretCommand :: ParsingState -> Command -> ParsingState
interpretCommand (_, f) (Cd {cdPath = "/"}) = ([], f)
interpretCommand (path, f) (Cd {cdPath = ".."}) = (init path, f)
interpretCommand (path, f) (Cd {cdPath = p}) = (path ++ [p], f)
interpretCommand (path, f) (Ls {lsRes = res}) = (path, updateTree path f res)

interpretCommands :: [Command] -> File
interpretCommands cs = snd $ foldl interpretCommand ([], Dir {dirName = "/", children = []}) cs

modifyAt :: a -> Int -> [a] -> [a]
modifyAt new idx xs = let (h,t) = splitAt idx xs in h ++ [new] ++ tail t

fileifyLs :: LsResult -> File
fileifyLs (LsDir {lsDirName = name}) = Dir {dirName = name, children = []}
fileifyLs (LsFile {lsFileSize = size, lsFileName = name}) = File {fileSize = size, fileName = name}

part1 :: Indata -> Int
part1 indata = let finalTree = interpretCommands indata in sum . fmap dirSize . filter ((<=100000) . dirSize) $ findDirs finalTree

findDirs :: File -> [File]
findDirs (File {}) = []
findDirs d@(Dir {children = c}) = d:(c >>= findDirs)

dirSize :: File -> Int
dirSize (File {fileSize = size}) = size
dirSize (Dir {children = c}) = sum $ fmap dirSize c

part2 :: Indata -> Int
part2 indata = minimum . filter (>=toDelete) . fmap dirSize $ findDirs f
    where
        f = interpretCommands indata
        toDelete = dirSize f - 40000000
