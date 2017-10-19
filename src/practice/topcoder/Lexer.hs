module Lexer where

import Data.List
import Data.Ord

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

longest :: [String] -> String
longest = maximumBy (comparing length)

-- input begins with one of the tokens
-- longest $ filter (\x -> prefix x s) ts

tokenize :: [String] -> String -> [String]
tokenize ts s = go ts s []

go :: [String] -> String -> [String] -> [String]
go ts s consumed
     | null s = consumed
     | null prefixes = go ts (tail s) consumed
     | otherwise = go ts dts (c : consumed) 
      where
        c = take longestPrefixLen s
        dts = drop longestPrefixLen s -- TODO: just partition
        longestPrefixLen = (length $ longest prefixes)
        prefixes = filter (\x -> prefix x s) ts

--  tokenize  ["ab","aba","A"] "ababbbaAab"

-- tokenize ["a","a","aa","aaa","aaaa","aaaaa","aa"] "aaaaaaaaaaaaaaaaaaaaaaaaa"

-- tokenize ["wow","wo","w"] "awofwwofowwowowowwwooo"
