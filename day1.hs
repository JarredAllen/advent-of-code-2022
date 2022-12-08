module Day1 where

import Data.List.Split (splitOn)
import Data.List (sortOn)
import System.IO

data Inventory = Inventory [[Integer]]

puzzle1 :: Inventory -> Integer
puzzle1 (Inventory meals) = (foldr max 0) $ map (foldr (+) 0) meals

puzzle2 :: Inventory -> Integer
puzzle2 (Inventory meals) = let
        elves = sortOn (0-) $ map (foldr (+) 0) meals
    in
        foldr (+) 0 $ take 3 elves

---------------------------------------- Glue ----------------------------------------

main :: IO ()
main = do 
    input <- parseInput
    let easy = puzzle1 input
    putStr "Easy: "
    print easy
    let hard = puzzle2 input
    putStr "Hard: "
    print hard

---------------------------------------- Parsing ----------------------------------------

parseInput :: IO Inventory
parseInput = do
    file <- openFile "day1.in" ReadMode
    contents <- hGetContents file
    return $ Inventory $ map parseElf $ splitOn "\n\n" contents

parseElf :: String -> [Integer]
parseElf = map read . lines

-----------------------------------------------------------------------------------------
