module Day9 where

import System.IO
import qualified Data.Set as Set

data Position = Position Integer Integer
    deriving (Show, Eq, Ord)
data Map = Map (Set.Set Position)
    deriving Show
data Rope = Rope [Position]
    deriving Show
data Move = Up | Down | Left | Right
    deriving Show

countVisited :: Map -> Integer
countVisited (Map places) = fromIntegral $ Set.size places

movePosition :: Move -> Position -> Position
movePosition Up (Position x y) = Position x $ y+1
movePosition Down (Position x y) = Position x $ y-1
movePosition Day9.Left (Position x y) = Position (x-1) y
movePosition Day9.Right (Position x y) = Position (x+1) y

updateTail :: Position -> Position -> Position
updateTail (Position headx heady) tail@(Position tailx taily) = let
        length = max (abs (headx - tailx)) (abs (heady - taily))
        moveFunc = case (headx-tailx, heady - taily) of
            -- If the head and tail are already next to each other, no move
            (-1,-1) -> id
            (-1,0) -> id
            (-1,1) -> id
            (0,-1) -> id
            (0,0) -> id
            (0,1) -> id
            (1,-1) -> id
            (1,0) -> id
            (1,1) -> id
            -- If 2 spaces in a straight line, move in that direction
            (2,0) -> movePosition Day9.Right
            (-2,0) -> movePosition Day9.Left
            (0,2) -> movePosition Up
            (0,-2) -> movePosition Down
            -- If diagonal, move in that diagonal
            -- Up Left
            (-2,1) -> (movePosition Day9.Left) . (movePosition Up)
            (-2,2) -> (movePosition Day9.Left) . (movePosition Up)
            (-1,2) -> (movePosition Day9.Left) . (movePosition Up)
            -- Down Left
            (-2,-1) -> (movePosition Day9.Left) . (movePosition Down)
            (-2,-2) -> (movePosition Day9.Left) . (movePosition Down)
            (-1,-2) -> (movePosition Day9.Left) . (movePosition Down)
            -- Up Right
            (2,1) -> (movePosition Day9.Right) . (movePosition Up)
            (2,2) -> (movePosition Day9.Right) . (movePosition Up)
            (1,2) -> (movePosition Day9.Right) . (movePosition Up)
            -- Down Right
            (2,-1) -> (movePosition Day9.Right) . (movePosition Down)
            (2,-2) -> (movePosition Day9.Right) . (movePosition Down)
            (1,-2) -> (movePosition Day9.Right) . (movePosition Down)
            _ -> undefined
        in moveFunc tail

updateFollowing :: Position -> [Position] -> [Position]
updateFollowing _ [] = []
updateFollowing head (follow:tail) = let
        new_follow = updateTail head follow
        new_tail = updateFollowing new_follow tail
    in new_follow:new_tail

doMove :: Move -> (Map, Rope) -> (Map, Rope)
doMove move (Map map, Rope (head:tail)) = let
        new_head = movePosition move head
        new_tail = updateFollowing new_head tail
        new_map = Map $ Set.insert (last new_tail) map 
    in (new_map, Rope (new_head:new_tail))

puzzle :: Int -> [Move] -> Integer
puzzle ropeLength moves = let
        rope = Rope $ take ropeLength $ repeat $ Position 0 0
        (final_map, _) = foldl (\x y -> doMove y x) (Map $ Set.empty, rope) moves
    in countVisited final_map

puzzle1 :: [Move] -> Integer
puzzle1 = puzzle 2

puzzle2 :: [Move] -> Integer
puzzle2 = puzzle 10

---------------------------------------- Glue ----------------------------------------

main :: IO ()
main = do 
    moves <- parseInput
    let start_rope = Rope [Position 0 0, Position 0 0]
    let (final_map, final_rope) = foldl (\x y -> doMove y x) (Map $ Set.empty, start_rope) moves
    -- print final_rope
    -- print final_map
    let easy = puzzle1 moves
    putStr "Easy: "
    print easy
    let hard = puzzle2 moves
    putStr "Hard: "
    print hard

---------------------------------------- Parsing ----------------------------------------

parseInput :: IO [Move]
parseInput = do
    file <- openFile "day9.in" ReadMode
    contents <- hGetContents file
    let parsedInput = map parseMoveLine $ lines contents
    return $ concatMap (\(move, count) -> take count $ repeat move) parsedInput

parseMoveLine :: String -> (Move, Int)
parseMoveLine s = let
        [move_str, count_str] = words s
        count = read count_str
        move = case move_str of
            "R" -> Day9.Right
            "L" -> Day9.Left
            "U" -> Up
            "D" -> Down
    in (move, count)

-----------------------------------------------------------------------------------------
