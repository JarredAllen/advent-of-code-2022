module Day2 where

import System.IO
import qualified Data.Char as Char
import qualified Data.List as List
import Debug.Trace

import Util (parseByDoubleLine, splitList, getDelimited)

data Packet = Num Int | PacketList [Packet]
    deriving Show
instance Eq Packet where
    (==) (Num x) (Num y) = x == y
    (==) (PacketList x) (PacketList y) = x == y
    (==) (Num x) (PacketList y) = [Num x] == y
    (==) (PacketList x) (Num y) = x == [Num y]
instance Ord Packet where
    (<=) (Num x) (Num y) = x <= y
    (<=) (PacketList x) (PacketList y) = x <= y
    (<=) (Num x) (PacketList y) = [Num x] <= y
    (<=) (PacketList x) (Num y) = x <= [Num y]
instance Read Packet where
    readsPrec _ s@('[':_) = let
            (inner, tail) = getDelimited s
        in trace (show s ++ ": " ++ show (inner, tail)) [(PacketList $ map read $ splitList (== ',') inner, tail)]
    readsPrec _ s = let
            lastIdx = List.findIndex (`elem` ",]") s
            num = case lastIdx of
                Just idx -> take idx s
                Nothing -> s
            tail = case lastIdx of
                Just idx -> drop idx s
                Nothing -> ""
        in trace (show s ++ ": " ++ show (num, tail))[(Num $ read num, tail)]

data PacketPair = PacketPair Packet Packet
    deriving Show

inRightOrder :: PacketPair -> Bool
inRightOrder (PacketPair left right) = left < right


puzzle1 :: [PacketPair] -> Int
puzzle1 = length . filter inRightOrder

puzzle2 :: [PacketPair]  -> Integer
puzzle2 = undefined

---------------------------------------- Glue ----------------------------------------

main :: IO ()
main = do 
    input <- parseInput
    print input
    let easy = puzzle1 input
    putStr "Easy: "
    print easy
    let hard = puzzle2 input
    putStr "Hard: "
    print hard

---------------------------------------- Parsing ----------------------------------------

parseInput :: IO [PacketPair]
parseInput = parseByDoubleLine parsePacketPair "day13.in"

parsePacketPair :: String -> PacketPair
parsePacketPair s = let
        split = lines s
        first = read $ split !! 0
        second = read $ split !! 1
    in PacketPair first second

-----------------------------------------------------------------------------------------
