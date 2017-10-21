{-# LANGUAGE OverloadedStrings #-}


module Whisper where

import Data.Strings
import Data.List.Split
import Data.List
import Data.Ord


-- String toWhom(String[] usernames, String typed)

toWhom :: [String] -> String -> String
toWhom us m =
  if strStartsWith m "/msg " then  findWhom us (m \\ msg)
  else  "not a whisper" 
  where
    msg = "/msg "

findWhom :: [String] -> String -> String
findWhom us m = maximumBy (comparing length) $ ll `intersect` us 
  where
    s = splitOn " " m
    r = tail $ subsequences s
    z = filter (\(x:xs) -> x == head s) r
    ll = map (concat . intersperse " ") z


{--

let us = ["John","John Doe","John Doe h"]
let m = "/msg John Doe hi there"

 strStartsWith m "/msg "

tail $ splitOn " " m

filter (\(x:xs) -> x == head r) (tail s)

init $ tail $ splitOn " " m -- TODO: instead of just init check if there a space at the end
-- Fix for a case like "/msg John Doe "

let ll = map (concat . intersperse " ") z

-> maximumBy (comparing length) $ ll `intersect` us

TODO: handle lowercase 
-}
