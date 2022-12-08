module Day5 where

import System.IO
import Text.Read

data Crates = Crates [[Char]] deriving Show
data Move = Move Int Int Int deriving Show

tops :: Crates -> String
tops (Crates c) = map head c

puzzle1 :: Crates -> [Move] -> String
puzzle1 initial_state moves = tops $ foldl doMove9000 initial_state moves

puzzle2 :: Crates -> [Move] -> String
puzzle2 initial_state moves = tops $ foldl doMove9001 initial_state moves

update :: Int -> a -> [a] -> [a]
update i y xs = take i xs ++ [y] ++ drop (i+1) xs

doMoveOrder :: (String -> String) -> Crates -> Move -> Crates
doMoveOrder func (Crates crates) (Move count source dest) = Crates new_list
    where
        (moved, new_source) = splitAt count (crates !! source)
        new_dest = func moved ++ (crates !! dest)
        new_list = update source new_source $ update dest new_dest crates

doMove9000 :: Crates -> Move -> Crates
doMove9000 = doMoveOrder reverse

doMove9001 :: Crates -> Move -> Crates
doMove9001 = doMoveOrder id

---------------------------------------- Glue ----------------------------------------

main :: IO ()
main = do 
    let initial_state = Crates $ map reverse ["RGJBTVZ", "JRVL", "SQF", "ZHNLFVQG", "RQTJCSMW", "SWTCHF", "DZCVFNJ", "LGZDWRFQ", "JBWVP"]
    moves <- parseInput
    let easy = puzzle1 initial_state moves
    putStr "Easy: "
    print easy
    let hard = puzzle2 initial_state moves
    putStr "Hard: "
    print hard

---------------------------------------- Parsing ----------------------------------------

parseInput :: IO [Move]
parseInput = do
    file <- openFile "day5.in" ReadMode
    contents <- hGetContents file
    return $ map parseMove $ lines contents

parseMove :: String -> Move
parseMove s = case parseMoveMaybe s of
    Just m -> m
    Nothing -> undefined

parseMoveMaybe :: String -> Maybe Move
parseMoveMaybe str =
  case words str of
    ["move", x, "from", y, "to", z] ->
      do count <- readMaybe x
         source <- readMaybe y
         dest <- readMaybe z
         return $ Move count (source - 1) (dest - 1)
    _ -> Nothing
-----------------------------------------------------------------------------------------
