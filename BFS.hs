module BFS (bfs, bfsTarget) where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set

bfs :: Ord a => (a -> [a]) -> a -> Map.Map a Int
bfs neighborFunc start = bfsHelper neighborFunc 0 [start] Map.empty

bfsTarget :: Ord a => (a -> [a]) -> a -> a -> Int
bfsTarget neighborFunc start target = bfsHelperTarget neighborFunc 0 target [start] Set.empty

bfsHelper :: Ord a => (a -> [a]) -> Int -> [a] -> Map.Map a Int -> Map.Map a Int
bfsHelper _ _ [] visited = visited
bfsHelper neighborFunc distance frontier visited = let
        newVisited = foldr (\a v -> Map.insert a distance v) visited frontier
        newFrontier = filter (not . (`Map.member` newVisited)) $ List.concatMap neighborFunc frontier
    in bfsHelper neighborFunc (distance+1) newFrontier newVisited

bfsHelperTarget :: Ord a => (a -> [a]) -> Int -> a -> [a] -> Set.Set a -> Int
bfsHelperTarget _ _ _ [] _ = error "Target not found"
bfsHelperTarget neighborFunc distance target frontier visited = let
        newVisited = Set.union visited $ Set.fromList frontier
        newFrontier = filter (not . (`Set.member` newVisited)) $ List.concatMap neighborFunc frontier
    in if target `elem` frontier
        then distance
        else bfsHelperTarget neighborFunc (distance+1) target newFrontier newVisited
