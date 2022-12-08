module Day3 where

import System.IO
import qualified Data.Set as Set
import qualified Data.Char as Char

data Sack = Sack (Set.Set Char) (Set.Set Char)
data Trio = Trio Sack Sack Sack

find_duplicate :: Sack -> Char
find_duplicate (Sack first second) = head $ Set.toList $ Set.intersection first second

get_all :: Sack -> Set.Set Char
get_all (Sack a b) = Set.union a b

find_badge :: Trio -> Char
find_badge (Trio x y z) = 
    head $ Set.toList $ Set.intersection (Set.intersection (get_all x) (get_all y)) (get_all z)

priority :: Char -> Int
priority c = if val > 96 then val - Char.ord 'a' + 1 else val - Char.ord 'A' + 27
    where val = Char.ord c

makeTrios :: [Sack] -> [Trio]
makeTrios [] = []
makeTrios (x:y:z:xs) = Trio x y z : makeTrios xs

puzzle1 :: [Sack] -> Integer
puzzle1 = foldr1 (+) . map fromIntegral . map priority . map find_duplicate

puzzle2 :: [Trio] -> Integer
puzzle2 = foldr1 (+) . map fromIntegral . map priority . map find_badge

---------------------------------------- Glue ----------------------------------------

main :: IO ()
main = do 
    input <- parseInput
    let easy = puzzle1 input
    putStr "Easy: "
    print easy
    let hard = puzzle2 $ makeTrios input
    putStr "Hard: "
    print hard

---------------------------------------- Parsing ----------------------------------------

parseInput :: IO [Sack]
parseInput = do
    file <- openFile "day3.in" ReadMode
    contents <- hGetContents file
    return $ map parseSack $ lines contents

parseSack :: String -> Sack
parseSack s = Sack (Set.fromList first) (Set.fromList second)
    where n = length s `div` 2
          (first, second) = splitAt n s
-----------------------------------------------------------------------------------------
