module Day2 where

import System.IO
import qualified Data.Char as Char
import qualified Data.List as List
import Text.Parsec
import Text.Parsec.Char as Parsec.Char
import Text.ParserCombinators.Parsec

import Util (parseByDoubleLine, splitList, getDelimited, indexed)

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

isDivider :: Int -> Packet -> Bool
isDivider num (PacketList [PacketList [Num num2]]) = num == num2
isDivider _ _ = False

divider :: Int -> Packet
divider num = PacketList [PacketList [Num num]]

data PacketPair = PacketPair Packet Packet
    deriving Show

inRightOrder :: PacketPair -> Bool
inRightOrder (PacketPair left right) = left < right

flatten :: [PacketPair] -> [Packet]
flatten [] = []
flatten (PacketPair left right:xs) = left:right:flatten xs

puzzle1 :: [PacketPair] -> Int
puzzle1 = sum . map (\(idx,_) -> idx) . filter (\(_, pair) -> inRightOrder pair) . indexed

puzzle2 :: [PacketPair]  -> Integer
puzzle2 pairs = let
        packets = List.sort $ divider 2:divider 6:flatten pairs
        fstIdx = case List.findIndex (isDivider 2) packets of
            Just idx -> fromIntegral idx
            Nothing -> error "Couldn't find first distress packet"
        sndIdx = case List.findIndex (isDivider 6) packets of
            Just idx -> fromIntegral idx
            Nothing -> error "Couldn't find first distress packet"
    in (fstIdx+1) * (sndIdx+1)

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

parseInput :: IO [PacketPair]
parseInput = parseByDoubleLine parsePacketPair "day13.in"

parsePacketPair :: String -> PacketPair
parsePacketPair s = let
        packets = lines s
        fst = case parse parsePacket "" $ packets !! 0 of
            Right packet -> packet
            Left err -> error $ "Failed to parse first packet " ++ show err ++ " from " ++ show s
        snd = case parse parsePacket "" $ packets !! 1 of
            Right packet -> packet
            Left err -> error $ "Failed to parse second packet " ++ show err ++ " from " ++ show s
    in PacketPair fst snd

parsePacket :: Parser Packet
parsePacket = parseNumPacket <|> parseListPacket

parseNumPacket :: Parser Packet
parseNumPacket = Num <$> read <$> many1 digit

parseListPacket :: Parser Packet
parseListPacket = do
    items <- between (Parsec.Char.char '[') (Parsec.Char.char ']') $ sepBy parsePacket (Parsec.Char.char ',')
    return $ PacketList items

-----------------------------------------------------------------------------------------
