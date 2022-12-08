module Day6 where

import System.IO
import qualified Data.List as List

data Signal = Signal String

windows :: Int -> Signal -> [String]
windows n (Signal s) = map (take n) $ List.tails s

combinator :: (a -> b) -> (a -> b -> c) -> a -> c
combinator map binary input = binary input $ map input

uniqueLetters :: String -> Bool
uniqueLetters = combinator List.nub (==)

firstDistinct :: Int -> Signal -> Maybe Int
firstDistinct n = (fmap (+n)) . (List.findIndex uniqueLetters) . (windows n)

puzzle1 :: Signal -> Maybe Int
puzzle1 = firstDistinct 4

puzzle2 :: Signal -> Maybe Int
puzzle2 = firstDistinct 14

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

parseInput :: IO Signal
parseInput = do
    file <- openFile "day6.in" ReadMode
    contents <- hGetContents file
    return $ Signal contents

-----------------------------------------------------------------------------------------
