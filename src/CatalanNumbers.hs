module CatalanNumbers where

import Test.QuickCheck (quickCheck)

-- helpers
binom :: Integer -> Integer -> Integer
binom n k = product [k + 1..n] `div` product [1..n - k]

factorial :: Integer -> Integer
factorial = product . enumFromTo 2

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | x <- xs, y <- xs]

--

cats1 = map (\n -> product [n+2..2*n] `div` product [1..n]) [0..]

cats2 = 1 : map (\n -> sum $ zipWith (*) (reverse (take n cats2)) cats2) [1..]

-- Catalan triangle

cats_triangle :: Integer -> Integer -> Integer
cats_triangle n k
  | k > n = 0
  | k < 0 = 0
  | otherwise = (binom (n+k) n * fromIntegral (n - k + 1)) `div` fromIntegral (n+1)


-- How many unique binary search trees can be made from a series of numbers 1, 2, 3, 4, …, n, for any given n?
-- that's can be calculated using catalan number 


-- TODO: number of ways to parenthesis an expression



-- TODO: catalan numbers topcoder



-- TODO: number of sortable permutations of length n is Cn


