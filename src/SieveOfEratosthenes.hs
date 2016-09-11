
module SieveOfErastothenes where

import Data.List

-- first simple method
-- primes = sieve [2..]
-- sieve (p : xs) = p : sieve [x | x <− xs, x `mod` p > 0]


primesTo m = sieve [2..m]
  where sieve (x:xs) = x : sieve (xs \\ [x, x+x..m])
        sieve [] = []

-- TODO: exercies here https://wiki.haskell.org/Prime_numbers 
