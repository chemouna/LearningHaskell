
module LetUsage where

import Prelude

-- example of different of writing a fn using let .. in .. and where
solveLogic :: Int -> Int -> Int
solveLogic a b =
  let
     x = 1
     myfn
        | a >= x = 1
        | a == b = 333
        | otherwise = 5
  in
     myfn

-- to get of redundunt use of myfn -> let's use where
solveLogicW :: Int -> Int -> Int
solveLogicW a b
         | a >= x = 1
         | a == b = 333
         | otherwise  = 5
       where x = 1

-- another way using case
solveLogicC :: Int -> Int -> Int
solveLogicC a b =
  let x = 1
      case () of
        _ | a >= x -> 1
          | a == b -> 333
          | otherwise -> 5


