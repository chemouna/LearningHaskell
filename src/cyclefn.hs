module Cyclefn where

import Data.Numbers.Primes
import Control.Monad

-- take 7 $ cycle [1..4]

{--
 Example 1: Euler 35 : Circular primes

 https://projecteuler.net/problem=35
 The number, 197, is called a circular prime because all
 rotations of the digits: 197, 971, and 719, are themselves
 prime.

 How many circular primes are there below one million ?

-}

circular p = all isPrime nums
  where
    ps = show p
    l = length ps
    nums = (map (\n -> (read . (take l) . (drop n)) (cycle ps)) [0..l-1])

solution35 = length $ filter circular candidates
      where candidates = takeWhile (< 10^6) primes
-- result = 55


{--
Example 2: Define a function funkyMap :: (a -> b) -> (a -> b) -> [a] -> [b]
that takes as arguments two functions f and g and a list xs, and applies f
to all elements at even positions [0, 2..] in xs and g to all elements at
odd positions [1, 3..] in xs.

-}

funkyMap f g = zipWith ($) (cycle [f, g])

-- cycle given a list repeats it infinitely, so this creates a non-ending list of [f, g, f, g, f ...]
-- $ given a function and an argument, applies the function to the argument.
-- So we create a list of infinite repetitions of [f, g] repeated and applies f to the first, g to the second,
--  f to the thrid, g to the forth ...


-- Example 3: Double every 2nd item in a list
mapSecond :: (a -> a) -> [a] -> [a]
mapSecond f = zipWith ($) (cycle [id, f])

doubleSecondList = mapSecond (* 2)

-- doubleSecondList [1, 2, 3, 4, 5]

{--
Example 4: Printing patterns
print this pattern :
................................................
.+..+..+..+..+..+..+..+..+..+..+..+..+..+..+..+.
+.++.++.++.++.++.++.++.++.++.++.++.++.++.++.++.+
................................................

48
. -> a
+ -> b

.+. -> a, b, a
+.+ -> b, a, b
...  -> a, ...
-}

pattern :: Int -> a -> a -> [[a]]
pattern len a b = map (take len . cycle) [[a], [a, b, a], [b, a, b], [a]]

printPattern :: IO ()
printPattern = putStr (unlines (pattern 48 '.' '+'))
