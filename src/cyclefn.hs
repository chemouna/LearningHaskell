module Cyclefn where

import Data.Numbers.Primes

{--
take 7 $ cycle [1..4]

-}


{--
 Example: Euler 35 : Circular primes

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


