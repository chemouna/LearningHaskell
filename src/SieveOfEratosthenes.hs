module SieveOfErastothenes where

import Control.Monad
import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Data.Foldable
import Data.List

-- first simple method
-- primes = sieve [2..]
-- sieve (p : xs) = p : sieve [x | x <− xs, x `mod` p > 0]


primesTo m = sieve [2..m]
  where sieve (x:xs) = x : sieve (xs \\ [x, x+x..m])
        sieve [] = []

-- with ST monad
primesUpTo :: Int -> [Int]
primesUpTo n = [p | (p, True) <- assocs $ sieve n]

sieve :: Int -> UArray Int Bool
sieve n = runSTUArray $ do
  sieve <- newArray (2, n) True
  forM_ [2..n] $ \p -> do
    isPrime <- readArray sieve p
    when isPrime $ do
      forM_ [p*2, p*3 .. n] $ \k -> do
        writeArray sieve k False
  return sieve


-- TODO: exercies here https://wiki.haskell.org/Prime_numbers
