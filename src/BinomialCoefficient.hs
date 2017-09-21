
module BinomialCoefficient where

-- This is a bad version for efficiency
fact 0 = 1
fact n = n * fact (n - 1)
choose n k = (fact n) `div` ((fact k)*(fact (n-k)))

-- another solution using recurrence
choose' n 0 = 1
choose' 0 k = 0
choose' n k = (choose (n - 1) (k - 1)) * (n `div` k)
