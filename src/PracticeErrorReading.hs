{-# LANGUAGE TypeFamilies #-}

module PracticeErrorReading where

-- http://ics.p.lodz.pl/~stolarek/_media/pl:research:stolarek_understanding_basic_haskell_error_messages.pdf

import Data.Maybe
import Data.List
import Data.Char

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

--

sortWrapper xs = sort xs 

--

isEq :: Eq a => a -> a -> Bool
isEq x y = x == y

isEq2 :: (Eq a, Eq b, a ~ b) => a -> b -> Bool
isEq2 x y = x == y

--

getFirstLetter :: String -> Char
getFirstLetter = head

getFirstLetter2 :: String -> String
getFirstLetter2 xs = [head xs]

getFirstLetter3 :: String -> String
getFirstLetter3 = (: []) . head

--

asInt :: String -> Int
asInt = foldl (\acc x -> acc * 10 + digitToInt x) 0


