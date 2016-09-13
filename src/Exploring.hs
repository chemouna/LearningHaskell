module Exploring where


--pb 1 : Find the sum of all the even-valued terms in the Fibonacci sequence which
-- do not exceed one million.

prob1_sol1 :: Integer
prob1_sol1  = sum [x | x <- takeWhile (<= 1000000) fibs, even x]
  where fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- factorial n = factorial (n - 1) * n

zipMap1 :: [a -> b] -> [a] -> [b]
zipMap1 [] _ = []
zipMap1 _ [] = []
zipMap1 (f:fs)(x:xs) = (f x):(zipMap1 fs xs)

zipMap2 :: [a -> b] -> [a] -> [b]
zipMap2 fs xs = map fstOnSnd (zip fs xs)
  where fstOnSnd (f,x) = f x

zipMap3 :: [a -> b] -> [a] -> [b]
zipMap3 = zipWith ($)

-- and applying combinators : S = zipMap and K = repeat we can write map, zip, zipWith, zipWith3
s = zipWith ($)
k = repeat

mapT f xs = s (k f) xs
zipWithT f as bs     = s (mapT f as) bs
zipT                 = zipWithT (,)
zipWith3T f as bs cs = s (zipWithT f as bs) cs
