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
