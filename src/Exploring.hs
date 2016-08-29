module Exploring where


--pb 1 : Find the sum of all the even-valued terms in the Fibonacci sequence which
-- do not exceed one million.

prob1_sol1 :: Integer
prob1_sol1  = sum [x | x <- takeWhile (<= 1000000) fibs, even x]
  where fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- factorial n = factorial (n - 1) * n

