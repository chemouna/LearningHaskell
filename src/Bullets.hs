
module Bullets where

import Data.List
import Data.Maybe

{--

Gun -> bullet -> Set scratches (unique)

input : [Gun] (Gun = (Scratch mark, scratch on a bullet)) + marks on a bullet
output : index of the gun that fired the bullet

guns = ["| | | |","|| || |"," |||| "]
bullet = "|| || |"

let guns2 = {"||| |","| | || "}
let bullet2 = "|||| "


"| |||  |   |" -> split by space : [|, |||, |, |]
"| |||  |   |" ->   same         : [|, |||, |, |]
match, but
"| |||  |   |" -> split by space : [|, |||, |, |]
"||| |  |   |" -> same           : [|||, |, |, |]
do not.

"|| ||| | " -> [||, |||, |]
"| || |||" -> [|, ||, |||]

-> pos mod length

-> obtain all rotation of a string and then check if our str is in them

-> rotation of a string = rotation of a list 

-}

match_bullet :: [String] -> String -> (String -> Bool) -> Int
match_bullet guns bullet f = fromMaybe (-1 :: Int) im
  where
     im = findIndex f guns

match :: [String] -> String -> Int
match guns bullet = match_bullet guns bullet (\x -> not $ null (filter (== x) (rotations bullet)))

rotations str = map (\x -> rotate x str) [0..(length str - 1)]

rotate n xs = bs ++ as
  where (as, bs) = splitAt n xs


-- match ["||||", "|| || |", " |||| "] "|| || |" == 1

-- match ["||| |","| | || "] "|||| " == 0

-- match ["|| || ||","| | | | ","||||||||"] "||| ||| " == -1 

-- match [] "| | | |" == -1

-- match ["|| || ||","| | | | ","||| ||| ","||||||||"] "|| ||| |" == 2
