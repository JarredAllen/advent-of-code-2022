module Util (
        parseByLine,
        parseByDoubleLine,
        unpackListFromMapIndices,
        mapListByKey,
        updateList,
        safeListIndex,
        splitList,
        getDelimited,
        indexed
    ) where

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

updateList :: Int -> a -> [a] -> [a]
updateList i y xs = take i xs ++ [y] ++ drop (i+1) xs

safeListIndex :: Int -> [a] -> Maybe a
safeListIndex _ [] = Nothing
safeListIndex 0 (x:_) = Just x
safeListIndex idx (_:tail) = safeListIndex (idx - 1) tail

splitList :: (a -> Bool) -> [a] -> [[a]]
splitList predicate lst = case dropWhile predicate lst of
                            [] -> []
                            lst' -> head:splitList predicate tail
                                where (head, tail) = break predicate lst'

countElem :: (Eq a) => a -> [a] -> Int
countElem x = length . filter (== x)

indexed :: [a] -> [(Int, a)]
indexed = indexedHelper 1

indexedHelper :: Int -> [a] -> [(Int, a)]
indexedHelper _ [] = []
indexedHelper idx (h:tail) = (idx,h) : indexedHelper (idx+1) tail

-- String manipulation

-- For a string which begins with '[', '(', or '{', return the string contained
-- in the delimiter opening the string and the tail after that delimiter
getDelimited :: String -> (String, String)
getDelimited (c:s) = let
        (open, close) = case c of
                            '[' -> ('[', ']')
                            '(' -> ('(', ')')
                            '{' -> ('{', '}')
                            _ -> error $ "Argument must begin with '(', '[', or '{', not " ++ show c ++ "in argument " ++ show (c:s)
        isBalanced n = countElem open (take n s) + 1 == countElem close (take n s)
        close_idx = head $ filter isBalanced [1..length s+1]
    in (take (close_idx-1) s, drop (close_idx + 1) s)
getDelimited "" = error "Argument must be non-empty"
