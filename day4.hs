module Day4 where

import System.IO
import Text.ParserCombinators.Parsec

data Range = Range Integer Integer
data Pair = Pair Range Range

contains :: Range -> Range -> Bool
contains (Range astart aend) (Range bstart bend) = astart <= bstart && bend <= aend

contained :: Pair -> Bool
contained (Pair a b) = contains a b || contains b a

overlaps :: Pair -> Bool
overlaps (Pair (Range astart aend) (Range bstart bend)) = astart <= bend && bstart <= aend

puzzle1 :: [Pair] -> Integer
puzzle1 = fromIntegral . length . filter contained

puzzle2 :: [Pair] -> Integer
puzzle2 = fromIntegral . length . filter overlaps

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

parseInput :: IO [Pair]
parseInput = do
    file <- openFile "day4.in" ReadMode
    contents <- hGetContents file
    return $ map parsePair $ lines contents

parsePair :: String -> Pair
parsePair s = case parse parseRanges "" s of
    Left err -> undefined
    Right pair -> pair

parseRanges :: Parser Pair
parseRanges = do
  a <- read <$> many1 digit
  _ <- char '-'
  b <- read <$> many1 digit
  _ <- char ','
  c <- read <$> many1 digit
  _ <- char '-'
  d <- read <$> many1 digit
  return $ Pair (Range a b) (Range c d)

-----------------------------------------------------------------------------------------
