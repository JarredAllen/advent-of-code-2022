module Day10 where

import System.IO
import qualified Data.Map as Map
import qualified Data.List as List

data Instruction = Noop | Addx Integer
    deriving Show

puzzle1 :: Map.Map Integer Integer -> Integer
puzzle1 values = foldr1 (+) $ map (\x -> x * findValueAtTime values x) [20, 60, 100, 140, 180, 220]

puzzle2 :: Map.Map Integer Integer -> IO ()
puzzle2 values = do
    putStr $ drawRow values 1 ++ "\n"
    putStr $ drawRow values 41 ++ "\n"
    putStr $ drawRow values 81 ++ "\n"
    putStr $ drawRow values 121 ++ "\n"
    putStr $ drawRow values 161 ++ "\n"
    putStr $ drawRow values 201 ++ "\n"

getValuesOverTime :: [Instruction] -> Integer -> Integer -> Map.Map Integer Integer
getValuesOverTime [] _ _ = Map.empty
getValuesOverTime (Noop:tail) time reg = Map.insert (time+1) reg $ getValuesOverTime tail (time+1) reg
getValuesOverTime (Addx x:tail) time reg = Map.insert (time+2) (reg+x) $ getValuesOverTime tail (time+2) (reg+x)

findValueAtTime :: Map.Map Integer Integer -> Integer -> Integer
findValueAtTime map time = case Map.lookup time map of
    Just value -> value
    Nothing -> case Map.lookup (time - 1) map of
        Just value -> value
        Nothing -> error $ "Invalid time: " ++ show time

isPixelLit :: Integer -> Integer -> Bool
isPixelLit xPos regValue = case xPos - regValue of
    -1 -> True
    0 -> True
    1 -> True
    _ -> False

drawRow :: Map.Map Integer Integer -> Integer -> String
drawRow values startTime = map (\x -> if isPixelLit x $ findValueAtTime values (startTime + x) then '#' else '.') $ take 40 [0..]

---------------------------------------- Glue ----------------------------------------

main :: IO ()
main = do 
    input <- parseInput
    let values = Map.insert 1 1 $ getValuesOverTime input 1 1
    -- print values
    let easy = puzzle1 values
    putStr "Easy: "
    print easy
    putStr "Hard:\n"
    puzzle2 values

---------------------------------------- Parsing ----------------------------------------

parseInput :: IO [Instruction]
parseInput = do
    file <- openFile "day10.in" ReadMode
    contents <- hGetContents file
    return $ map parseGame $ lines contents

parseGame :: String -> Instruction
parseGame s = case take 4 s of
    "noop" -> Noop
    "addx" -> Addx $ read $ words s !! 1

-----------------------------------------------------------------------------------------
