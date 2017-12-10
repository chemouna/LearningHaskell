
module Billboard where

import Data.List.Split 
{--

Billboard displays letters 
multiple lightbulbs per letter

enlarged letter = 5x5 lightbulb 

element formtat : ":*****-*****-*****-*****-*****"

# : lit lightbulb
. : unlit lightbulb

Each group of 5 (delimited by a dash, '-') represents a row in the 5x5 representation of the letter.

T:#####-..#..-..#..-..#..-..#.." means that the 5x5 representation of 'T'

"#####"
"..#.."
"..#.."
"..#.."
"..#.."


Expl1:


"TOPCODER"

{"T:
  #####
  ..#..
  ..#..
  ..#..
  ..#..
,"O:
  #####
  #...#
  #...#
  #...#
  #####"
,"P:
  ####.
  #...#
  ####.
  #....
  #....
,"C:
  .####
  #....
  #....
  #....
  .####"
,"D:
  ####.
  #...#
  #...#
  #...#
  ####."
,"E:
  #####
  #....
  ####.
  #....
  #####"
,"R:
  ####.
  #...#
  ####.
  #.#..
  #..##"}


Returns: {

  "#####.#####.####...####.#####.####..#####.####."
  "..#...#...#.#...#.#.....#...#.#...#.#.....#...#"
  "..#...#...#.####..#.....#...#.#...#.####..####."
  "..#...#...#.#.....#.....#...#.#...#.#.....#.#.."
  "..#...#####.#......####.#####.####..#####.#..##" }

Simple exple :


let letters = ["T:#####-..#..-..#..-..#..-..#.." ,"O:#####-#...#-#...#-#...#-#####"]
Data.List.map tail xs
let xs = map (splitOn ":") letters

TODO: maybe do splitOns in one op 
-}


enlarge :: String -> [String] -> [String]
enlarge = undefined

