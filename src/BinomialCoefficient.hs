
module BinomialCoefficient where

-- This is a bad version for efficiency
fact 0 = 1
fact n = n * fact (n - 1)
choose n k = (fact n) `div` ((fact k)*(fact (n-k)))
