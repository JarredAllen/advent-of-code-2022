module Util (parseByLine, parseByDoubleLine, unpackListFromMapIndices, mapListByKey) where

import System.IO
import qualified Data.Map as Map

-- Parsing

parseByLine :: (String -> a) -> String -> IO [a]
parseByLine = parseFile lines

splitByDoubleLineHelper :: [String] -> (String, [String])
splitByDoubleLineHelper ("":[]) = ("", [])
splitByDoubleLineHelper ("":lst) = let (working, new_tail) = splitByDoubleLineHelper lst in ("", tail working:new_tail)
splitByDoubleLineHelper (last:[]) = ('\n':last, [])
splitByDoubleLineHelper (head:lst) = let (working, new_tail) = splitByDoubleLineHelper lst in ("\n" ++ head ++ working, new_tail)

splitByDoubleLine :: String -> [String]
splitByDoubleLine str = let (working, lst) = splitByDoubleLineHelper $ lines str in tail working:lst

parseByDoubleLine :: (String -> a) -> String -> IO [a]
parseByDoubleLine = parseFile splitByDoubleLine

parseFile :: (String -> [String]) -> (String -> a) -> String -> IO [a]
parseFile splitFunc mapFunc filename = do
    file <- openFile filename ReadMode
    contents <- hGetContents file
    return $ map mapFunc $ splitFunc contents

-- Data structure manipulation

unpackListFromMapIndices :: a -> Map.Map Int a -> [a]
unpackListFromMapIndices = unpackListFromMapIndicesHelper 0

unpackListFromMapIndicesHelper :: Int -> a -> Map.Map Int a -> [a]
unpackListFromMapIndicesHelper idx dflt map = let
        head = case Map.lookup idx map of
            Just value -> value
            Nothing -> dflt
        tail = unpackListFromMapIndicesHelper (idx + 1) dflt (Map.filterWithKey (\key _ -> key /= idx) map)
    in if Map.null map then [] else head:tail

mapListByKey :: Ord k => [(k, a)] -> Map.Map k [a]
mapListByKey [] = Map.empty
mapListByKey ((k,v):tail) = let
        recurse = mapListByKey tail
        lst = Map.findWithDefault [] k recurse
        newMap = Map.insert k (v:lst) recurse
    in newMap
