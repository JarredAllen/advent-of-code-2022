module Day2 where

import System.IO

data OppMove = Rock | Paper | Scissors
data MyMove = X | Y | Z;
data Game = Game OppMove MyMove

scoreEasy :: Game -> Integer
scoreEasy (Game Rock X) = 4
scoreEasy (Game Rock Y) = 8
scoreEasy (Game Rock Z) = 3
scoreEasy (Game Paper X) = 1
scoreEasy (Game Paper Y) = 5
scoreEasy (Game Paper Z) = 9
scoreEasy (Game Scissors X) = 7
scoreEasy (Game Scissors Y) = 2
scoreEasy (Game Scissors Z) = 6

scoreHard :: Game -> Integer
scoreHard (Game Rock X) = 3
scoreHard (Game Rock Y) = 4
scoreHard (Game Rock Z) = 8
scoreHard (Game Paper X) = 1
scoreHard (Game Paper Y) = 5
scoreHard (Game Paper Z) = 9
scoreHard (Game Scissors X) = 2
scoreHard (Game Scissors Y) = 6
scoreHard (Game Scissors Z) = 7

allScore :: (Game -> Integer) -> [Game] -> Integer
allScore score = foldr (+) 0 . map score

puzzle1 :: [Game] -> Integer
puzzle1 = allScore scoreEasy

puzzle2 :: [Game] -> Integer
puzzle2 = allScore scoreHard

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

parseInput :: IO [Game]
parseInput = do
    file <- openFile "day2.in" ReadMode
    contents <- hGetContents file
    return $ map parseGame $ lines contents

parseGame :: String -> Game
parseGame s = case s of
    "A X" -> Game Rock X
    "A Y" -> Game Rock Y
    "A Z" -> Game Rock Z
    "B X" -> Game Paper X
    "B Y" -> Game Paper Y
    "B Z" -> Game Paper Z
    "C X" -> Game Scissors X
    "C Y" -> Game Scissors Y
    "C Z" -> Game Scissors Z
    _ -> undefined

-----------------------------------------------------------------------------------------
