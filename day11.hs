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

puzzle :: (Op -> Integer -> Integer) -> Int -> Monkeys -> Integer
puzzle opFunc numRounds monkeys = let
        post_moves = List.iterate (stepMonkeys opFunc) monkeys !! numRounds
        activities = reverse $ List.sort $ getActivities post_moves
    in (activities !! 0) * (activities !! 1)

puzzle1 :: Monkeys -> Integer
puzzle1 = puzzle applyOpEasy 20

puzzle2 :: Monkeys -> Integer
puzzle2 = puzzle applyOpHard 10000

stepMonkey :: (Op -> Integer -> Integer) -> Monkeys -> Int -> Monkeys
stepMonkey opFunc (Monkeys monkeys items counts) idx = let
        monkey = monkeys !! idx
        thrownItems = items !! idx
        updateWorries = updateWorry opFunc monkey thrownItems
        withCondTrue = filter (checkCond $ condition monkey) updateWorries
        withCondFalse = filter (not . (checkCond $ condition monkey)) updateWorries
        newTrueTarget = (items !! trueTarget monkey) ++ withCondTrue
        newFalseTarget = (items !! falseTarget monkey) ++ withCondFalse
        newItems = updateList idx [] $ updateList (trueTarget monkey) newTrueTarget $ updateList (falseTarget monkey) newFalseTarget items
        newCounts = updateList idx ((fromIntegral . length) thrownItems + counts !! idx) counts
    in Monkeys monkeys newItems newCounts

stepMonkeys :: (Op -> Integer -> Integer) -> Monkeys -> Monkeys
stepMonkeys opFunc monkeys = foldl (stepMonkey opFunc) monkeys [0..length (getItems monkeys) - 1]

applyOpEasy :: Op -> Integer -> Integer
applyOpEasy Square = (\x -> x * x `div` 3)
applyOpEasy (Add x) = (`div` 3) . (+ x)
applyOpEasy (Mul x) = (`div` 3) . (* x)

-- mod by 223092870 because that preserves behavior while bounding number sizes, enabling computation
applyOpHard :: Op -> Integer -> Integer
applyOpHard Square = (\x -> x * x `mod` 223092870)
applyOpHard (Add x) = (`mod` 223092870) . (+ x)
applyOpHard (Mul x) = (`mod` 223092870) . (* x)

updateWorry :: (Op -> Integer -> Integer) -> Monkey -> [Integer] -> [Integer]
updateWorry opFunc monkey = map (opFunc (operation monkey))

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
    print $ getActivities $ List.iterate (stepMonkeys applyOpHard) monkeys !! 20
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
