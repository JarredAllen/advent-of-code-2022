module Day8 where

import System.IO
import qualified Data.List as List
import Data.Maybe

-- This list is guaramteed to be square
data Plot = Plot [[Integer]]
    deriving Show

-- Get the height of the tree at given coordinates
treeAt :: Plot -> (Integer, Integer) -> Integer
treeAt (Plot plot) (x,y) = plot !! fromIntegral y !! fromIntegral x

-- Get the vertical (north-south) length of the plot
plotHeight :: Plot -> Integer
plotHeight (Plot plot) = fromIntegral $ length plot

-- Get the horizontal (east-west) length of the plot
plotWidth :: Plot -> Integer
plotWidth (Plot plot) = maximum $ map (fromIntegral . length) plot

-- List all trees visible to the north, from nearest to farthest
treesNorth :: Plot -> (Integer, Integer) -> [Integer]
treesNorth plot (x,y) = reverse [treeAt plot (x,y') | y' <- [0..y-1]]

-- List all trees visible to the east, from nearest to farthest
treesEast :: Plot -> (Integer, Integer) -> [Integer]
treesEast plot (x,y) = reverse [treeAt plot (x',y) | x' <- [0..x-1]]

-- List all trees visible to the south, from nearest to farthest
treesSouth :: Plot -> (Integer, Integer) -> [Integer]
treesSouth plot (x,y) = [treeAt plot (x,y') | y' <- [y+1..plotHeight plot - 1]]

-- List all trees visible to the west, from nearest to farthest
treesWest :: Plot -> (Integer, Integer) -> [Integer]
treesWest plot (x,y) = [treeAt plot (x',y) | x' <- [x+1..plotWidth plot - 1]]

takeUntilFirstException :: (a -> Bool) -> [a] -> [a]
takeUntilFirstException func lst = let (while, tail) = span func lst in case tail of
    [] -> while
    elem:_ -> while ++ [elem]

-- Return true if the tree at the index is visible from outside the plot
treeVisible :: Plot -> (Integer, Integer) -> Bool
treeVisible plot loc = let
    height = treeAt plot loc
    north = treesNorth plot loc
    east = treesEast plot loc
    south = treesSouth plot loc
    west = treesWest plot loc
    northSight = case north of
        [] -> True
        heights -> maximum heights < height
    eastSight = case east of
        [] -> True
        heights -> maximum heights < height
    southSight = case south of
        [] -> True
        heights -> maximum heights < height
    westSight = case west of
        [] -> True
        heights -> maximum heights < height
    in northSight || eastSight || southSight || westSight

-- Return a list containing all indices of trees in the plot
treeIndices :: Plot -> [(Integer, Integer)]
treeIndices plot = [(x,y) | x <- [0..plotWidth plot - 1], y <- [0..plotHeight plot - 1]]

-- Returns the product of the number of trees visible in each direction
viewScore :: Plot -> (Integer, Integer) -> Integer
viewScore plot loc = let
        height = treeAt plot loc
        north = treesNorth plot loc
        east = treesEast plot loc
        south = treesSouth plot loc
        west = treesWest plot loc
        scoreFunc = fromIntegral . length . takeUntilFirstException (< height)
        northScore = scoreFunc north
        eastScore = scoreFunc east
        southScore = scoreFunc south
        westScore = scoreFunc west
    in northScore * eastScore * southScore * westScore

highestViewScore :: Plot -> Integer
highestViewScore plot = maximum $ map (viewScore plot) $ treeIndices plot

puzzle1 :: Plot -> Integer
-- Comment out the computation because it's slow
puzzle1 plot = 1779 -- fromIntegral $ length $ filter (treeVisible plot) $ treeIndices plot

puzzle2 :: Plot -> Integer
puzzle2 plot = maximum $ map (viewScore plot) $ treeIndices plot

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

parseInput :: IO Plot
parseInput = do
    file <- openFile "day8.in" ReadMode
    contents <- hGetContents file
    return $ Plot $ map (map (read . (:[]))) $ lines contents

-----------------------------------------------------------------------------------------
