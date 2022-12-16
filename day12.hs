module Day12 where

import System.IO
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map

import BFS (bfsTarget)
import Util (safeListIndex)

data Location = Location { x :: Int, y :: Int }
    deriving (Show, Eq, Ord)
data HeightMap = HeightMap { heights :: [[Integer]], start :: Location, end :: Location }
    deriving Show

optHeightAt :: HeightMap -> Location -> Maybe Integer
optHeightAt (HeightMap { heights, start = _, end = _ }) loc = do
    row <- safeListIndex (fromIntegral $ y loc) heights
    safeListIndex (fromIntegral $ x loc) row

heightAt :: HeightMap -> Location -> Integer
heightAt map loc = case optHeightAt map loc of
    Just x -> x
    Nothing -> error $ "Invalid index " ++ show loc ++ "for map:\n" ++ show map

inBounds :: HeightMap -> Location -> Bool
inBounds map loc = case optHeightAt map loc of
    Just _ -> True
    Nothing -> False

neighbors :: HeightMap -> Location -> [Location]
neighbors map location@(Location { x, y }) = let
        currentHeight = heightAt map location
        possibleNeighbors = [
                Location { x=x+1, y=y },
                Location { x=x-1, y=y },
                Location { x=x, y=y+1 },
                Location { x=x, y=y-1 }
            ]
        legalNeighbors = filter (inBounds map) possibleNeighbors
        neighbors = filter ((<= currentHeight + 1) . (heightAt map)) legalNeighbors
    in neighbors

puzzle1 :: HeightMap -> Integer
puzzle1 map = fromIntegral $ bfsTarget (neighbors map) (start map) (end map)

puzzle2 :: HeightMap -> Integer
puzzle2 = undefined

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

parseInput :: IO HeightMap
parseInput = do
    file <- openFile "day12.in" ReadMode
    contents <- hGetContents file
    return $ getHeightMap $ lines contents

getHeightMap :: [String] -> HeightMap
getHeightMap input = let
        heights = map (map (\c -> case c of
                'S' -> 0
                'E' -> 25
                _ -> fromIntegral $ Char.ord c - Char.ord 'a'
            )) input
        startY = case List.findIndex (elem 'S') input of
            Just idx -> idx
            Nothing -> error "Couldn't find start 'S'"
        startX = case List.elemIndex 'S' $ input !! startY of
            Just idx -> idx
            Nothing -> error "Couldn't find start 'S'"
        start = Location { x = fromIntegral startX, y = fromIntegral startY }
        endY = case List.findIndex (elem 'E') input of
            Just idx -> idx
            Nothing -> error "Couldn't find end 'E'"
        endX = case List.elemIndex 'E' $ input !! endY of
            Just idx -> idx
            Nothing -> error "Couldn't find end 'E'"
        end = Location { x = fromIntegral endX, y = fromIntegral endY }
    in HeightMap { heights = heights, start = start, end = end }

-----------------------------------------------------------------------------------------
