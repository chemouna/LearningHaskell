module LetUsage where

import Control.Monad.Writer
import Control.Monad
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
    case () of
      _ | a >= x -> 1
        | a == b -> 33
        | otherwise -> 5
     where x = 1

-- Using if in a let expression

-- foo :: Integer -> Integer -> [Integer]
-- foo a b = do
--             let result = []
--             let Coord x y = boo a b
--             if x > 0
--               let result = result ++ [3]
--             if y > 0
--               let result = result ++ [5]
--             if x < a
--               let result = result ++ [7]
--             if y < b
--               let result = result ++ [9]
--             result
-- the above foo function doesn't work -> a better way to write it

data Coord a b = Coord a b
boo = undefined

foo :: Integer -> Integer -> [Integer]
foo a b = map snd
           (filter fst [(x > 0, 3),
             (y > 0, 5),
             (x < a, 7),
             (y < b, 9)])
      where Coord x y = boo a b

-- another way
foo2 a b = let Coord x y = boo a b in
           (if x > 0 then (3 :) else id) .
           (if y > 0 then (5 :) else id) .
           (if x < a then (7 :) else id) .
           (if y < b then (9 :) else id) $
           []

-- with the writer monad
foo3 a b = execWriter $ do
             let Coord x y = boo a b
             when (x > 0) $ tell [3]
             when (y > 0) $ tell [5]
             when (x < a) $ tell [7]
             when (y < b) $ tell [9]
t
