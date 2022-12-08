module Day7 where

import System.IO
import qualified Data.List as List

data Path = Path [String] deriving Show
data File = Directory String [File] | File String Integer
    deriving Show
data LsEntry = LsFile String Integer | LsDirectory String
    deriving Show
data Cmd = Cd String | CdUp | CdHome | Ls [LsEntry]
    deriving Show
data CliLine = CashCd String | CashCdUp | CashCdHome | CashLs | FileList Integer String | DirectoryList String
    deriving Show

directorySize :: File -> Integer
directorySize (File _ size) = size
directorySize (Directory _ dirents) = foldr1 (+) $ map directorySize dirents

iterateDirectories :: File -> [File]
iterateDirectories (File _ _) = []
iterateDirectories (Directory name dirents) = (Directory name dirents):(foldr1 (++) $ map iterateDirectories dirents)

puzzle1 :: File -> Integer
puzzle1 = foldr (+) 0 . filter (<= 100000) . map directorySize . iterateDirectories

puzzle2 :: File -> Integer
puzzle2 filesystem = let
        total_space = directorySize filesystem
        unused_space = 70000000 - total_space
        needed_space = 30000000 - unused_space
        sizes = map directorySize $ iterateDirectories filesystem
        eligible = filter (>= needed_space) sizes
    in minimum eligible

isCmd :: CliLine -> Bool
isCmd (FileList _ _) = False
isCmd (DirectoryList _) = False
isCmd _ = True

parseCmd :: [CliLine] -> (Cmd, [CliLine])
parseCmd ((CashCd s):tail) = (Cd s, tail)
parseCmd (CashCdUp:tail) = (CdUp, tail)
parseCmd (CashCdHome:tail) = (CdHome, tail)
parseCmd (CashLs:tail) = let
        lines = takeWhile (not . isCmd) tail
        trueTail = dropWhile (not . isCmd) tail
    in (Ls $ map mapLsOutput lines, trueTail)
  where
    mapLsOutput (DirectoryList s) = LsDirectory s
    mapLsOutput (FileList size s) = LsFile s size

parseCmds :: [CliLine] -> [Cmd]
parseCmds [] = []
parseCmds lines = let (cmd, tail) = parseCmd lines in cmd:parseCmds tail

hasName :: String -> File -> Bool
hasName query (Directory name _) = query == name
hasName query (File name _) = query == name

getPath :: File -> Path -> File
getPath f (Path []) = f
getPath (Directory _ dirents) (Path path) = let
        headSegment = last path
        tailSegments = init path
        dirent = head $ filter (hasName headSegment) dirents
    in getPath dirent $ Path tailSegments

appendEntries :: [LsEntry] -> File -> File
appendEntries [] f = f
appendEntries ((LsFile fname size):entries) (Directory dname contents) = appendEntries entries $ Directory dname $ (File fname size):contents
appendEntries ((LsDirectory sname):entries) (Directory dname contents) = appendEntries entries $ Directory dname $ (Directory sname []):contents

insertAtPath :: File -> Path -> File -> File
insertAtPath f (Path []) _ = f
insertAtPath f (Path path) (Directory dname dirents) = let
        headSegment = last path
        tailSegments = init path
        dirent_index = List.findIndex (hasName headSegment) dirents
        dirent = case dirent_index of
            Just idx -> dirents !! idx
            Nothing -> Directory headSegment []
        new_dirent = insertAtPath f (Path tailSegments) dirent
        new_dirents = case dirent_index of
            Just idx -> update idx new_dirent dirents
            Nothing -> new_dirent:dirents
    in Directory dname new_dirents

update :: Int -> a -> [a] -> [a]
update i y xs = take i xs ++ [y] ++ drop (i+1) xs

buildTree :: (File, Path) -> Cmd -> (File, Path)
buildTree (f, Path ps) CdHome = (f, Path [])
buildTree (f, Path []) CdUp = undefined
buildTree (f, Path ps) CdUp = (f, Path $ tail ps)
buildTree (f, Path ps) (Cd s) = (f, Path (s:ps))
buildTree (File _ _, Path ps) (Ls entries) = undefined
buildTree (dir, path) (Ls entries) = let
        work_dir = getPath dir path
        new_work_dir = appendEntries entries work_dir
        new_tree = insertAtPath new_work_dir path dir
    in (new_tree, path)

---------------------------------------- Glue ----------------------------------------

main :: IO ()
main = do 
    input <- parseInput
    let cmds = parseCmds input
    let (tree, final_path) = foldl buildTree (Directory "/" [], Path []) cmds
    let easy = puzzle1 tree
    putStr "Easy: "
    print easy
    let hard = puzzle2 tree
    putStr "Hard: "
    print hard

---------------------------------------- Parsing ----------------------------------------

parseInput :: IO [CliLine]
parseInput = do
    file <- openFile "day7.in" ReadMode
    contents <- hGetContents file
    return $ map parseGame $ lines contents

parseGame :: String -> CliLine
parseGame s = case s of
    "$ ls" -> CashLs
    "$ cd .." -> CashCdUp
    "$ cd /" -> CashCdHome
    _ -> if take 5 s == "$ cd "
            then CashCd $ drop 5 s
         else
            if take 4 s == "dir "
                then DirectoryList $ drop 4 s
            else
                let
                    split = words s
                    size = read (split !! 0)
                    name = split !! 1
                in FileList size name

-----------------------------------------------------------------------------------------
