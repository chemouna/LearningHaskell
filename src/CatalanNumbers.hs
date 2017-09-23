
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

catal1 :: Int -> Int
catal1 0 = 1
catal1 n = ((2 * (2 * n - 1)) `div` (n + 1)) * catal1 (n - 1)

-- take 10 (map catal1 [1..])
