module Day2 where

import System.IO
import qualified Data.Map as Map
import qualified Data.List as List

import Util (parseByLine, parseByDoubleLine, unpackListFromMapIndices, mapListByKey, updateList)

data Monkeys = Monkeys [Monkey] [[Integer]] [Integer]
    deriving Show

data Monkey = Monkey { operation :: Op,
                       condition :: Cond,
                       trueTarget :: Int,
                       falseTarget :: Int
                     }
    deriving Show
data Op = Add Integer | Mul Integer | Square
    deriving Show
data Cond = Divisible Integer
    deriving Show

puzzle1 :: Monkeys -> Integer
puzzle1 monkeys = let
        post_moves = foldl (\monkeys _ -> stepMonkeys monkeys) monkeys [1..20]
        activities = reverse $ List.sort $ getActivities post_moves
    in (activities !! 0) * (activities !! 1)

puzzle2 :: Monkeys -> Integer
puzzle2 = undefined

stepMonkey :: Monkeys -> Int -> Monkeys
stepMonkey (Monkeys monkeys items counts) idx = let
        monkey = monkeys !! idx
        thrownItems = items !! idx
        updateWorries = updateWorry monkey thrownItems
        withCondTrue = filter (checkCond $ condition monkey) updateWorries
        withCondFalse = filter (not . (checkCond $ condition monkey)) updateWorries
        newTrueTarget = (items !! trueTarget monkey) ++ withCondTrue
        newFalseTarget = (items !! falseTarget monkey) ++ withCondFalse
        newItems = updateList idx [] $ updateList (trueTarget monkey) newTrueTarget $ updateList (falseTarget monkey) newFalseTarget items
        newCounts = updateList idx ((fromIntegral . length) thrownItems + counts !! idx) counts
    in Monkeys monkeys newItems newCounts

stepMonkeys :: Monkeys -> Monkeys
stepMonkeys monkeys = foldl stepMonkey monkeys [0..length (getItems monkeys) - 1]

applyOp :: Op -> Integer -> Integer
applyOp Square = (\x -> x * x `div` 3)
applyOp (Add x) = (`div` 3) . (+ x)
applyOp (Mul x) = (`div` 3) . (* x)

updateWorry :: Monkey -> [Integer] -> [Integer]
updateWorry monkey = map (applyOp (operation monkey))

checkCond :: Cond -> Integer -> Bool
checkCond (Divisible x) = (== 0) . (`mod` x)

getTarget :: Monkey -> Integer -> Int
getTarget monkey item = if checkCond (condition monkey) item then trueTarget monkey else falseTarget monkey

getItems :: Monkeys -> [[Integer]]
getItems (Monkeys _ items _) = items

getActivities :: Monkeys -> [Integer]
getActivities (Monkeys _ _ activities) = activities

---------------------------------------- Glue ----------------------------------------

main :: IO ()
main = do 
    input <- parseInput
    let monkeys = makeMonkeys input
    let easy = puzzle1 monkeys
    putStr "Easy: "
    print easy
    let hard = puzzle2 monkeys
    putStr "Hard: "
    print hard

---------------------------------------- Parsing ----------------------------------------

makeMonkeys :: [(Monkey, [Integer])] -> Monkeys
makeMonkeys lst = let
        monkeys = map fst lst
        items = map snd lst
    in Monkeys monkeys items [0 | _ <- items]

parseInput :: IO [(Monkey, [Integer])]
parseInput = parseByDoubleLine parseMonkey "day11.in"

parseMonkey :: String -> (Monkey, [Integer])
parseMonkey s = let
        split = lines s

        itemsLines = split !! 1
        itemsList = map (\s -> read $ if elem ',' s then init s else s) $ drop 2 $ words itemsLines

        opLine = split !! 2
        opWords = words opLine
        op = case (last $ init opWords, last opWords) of
            ("*", "old") -> Square
            ("*", num) -> Mul $ read num
            ("+", num) -> Add $ read num
            _ -> undefined

        condLine = split !! 3
        cond = Divisible $ read $ last $ words condLine

        trueTargetLine = split !! 4
        true = read $ last $ words trueTargetLine

        falseTargetLine = split !! 5
        false = read $ last $ words falseTargetLine
    in (Monkey { operation = op, condition = cond, trueTarget = true, falseTarget = false }, itemsList)

-----------------------------------------------------------------------------------------
