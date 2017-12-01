
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
" | || |||" -> [|, ||, |||]

-> pos mod length 

-}

match_bullet :: [String] -> String -> (String -> Bool) -> Int
match_bullet guns bullet f = fromMaybe (-1 :: Int) im
  where
     im = findIndex f guns

-- f = (== bullet)

match :: [String] -> String -> Int
match guns bullet = undefined -- match_bullet ()
