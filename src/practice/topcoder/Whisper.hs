

module Whisper where

import Data.List.Split

-- String toWhom(String[] usernames, String typed)

{--
toWhom :: [String] -> String -> String
toWhom
  | (msg:rest) = findWhom rest
  | "not a whisper"  
  where
    msg = "/msg "
    findWhom rest
     | () 
--}
{--

let us = ["John","John Doe","John Doe h"]
let m = "/msg John Doe hi there"

 strStartsWith m "/msg "

tail $ splitOn " " m

filter (\(x:xs) -> x == head r) (tail s)

init $ tail $ splitOn " " m -- TODO: instead of just init check if there a space at the end
-- Fix for a case like "/msg John Doe "

let r = tail $ subsequences s

let z = filter (\(x:xs) -> x == head s) r

let ll = map (concat . intersperse " ") z

-> maximumBy (comparing length) $ ll `intersect` us

TODO: handle lowercase 
-}
