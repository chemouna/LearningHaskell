
module DivisibilityCriteria where

toDigits :: Int -> [Int]
toDigits = map (read . (:[])) . show

-- s±(A) = a(0) − a(1) +…+ (−1)^n . a(n)
sumDigitsForDivCriteria x = sum (zipWith (*) (cycle [1, -1]) (reverse (toDigits  x)))

isDivisibleBy11 x
  | x == 11 || x == 0 = True
  | x < 11 = False
  | x > 11 = isDivisibleBy11 (sumDigitsForDivCriteria x)
