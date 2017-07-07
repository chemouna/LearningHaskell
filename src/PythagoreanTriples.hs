module PythagoreanTriples where

-- get pythagorean triples without symetrical values
pyth_trip_non_sym :: Int -> [(Int, Int, Int)]
pyth_trip_non_sym n = [(a, b, c) | a <- [1..n], b <- [1..a], c <- [1..n], (a^2) + (b^2) == (c^2)]

-- an even more efficient way
pyth_trip_non_sym2 :: Int -> [(Int, Int, Int)]
pyth_trip_non_sym2 n = [(a, b, a^2 + b^2) | a <- [1..n], b <- [1..a], isSquare (a^2 + b^2)]

isSquare x = let x' = truncate $ sqrt (fromIntegral x :: Double) in x'*x' == x

-- using Tree of primitive pythagorean triples

branchTriple = [
   \(a,b,c) -> (a - 2*b + 2*c, 2*a - b + 2*c, 2*a - 2*b + 3*c),
   \(a,b,c) -> (a + 2*b + 2*c, 2*a + b + 2*c, 2*a + 2*b + 3*c),
   \(a,b,c) -> (2*b - a + 2*c, b - 2*a + 2*c, 2*b - 2*a + 3*c)
 ]

-- first find primitive triples
primitiveTriples l = pt (3,4,5) where
  pt x = x : concatMap pt (filter l $ map ($ x) branchTriple)

-- find all triples (which normaly are k primitive-triple)
allTriples l = let -- notice the use of let with multiple assignments here
  pt = primitiveTriples' l
  mtriple (a,b,c) n = (a*n, b*n, c*n)
  multiples p = takeWhile l $ map (mtriple p) [1..] -- [1..] is to generate lazily all the keys that we will use to multiple primitive triples and get all triples -> notice the use of laziness here
  in concatMap multiples pt

usage n = allTriples (\(a,b,c) -> all (<= n) [a,b,c])

-- another way we can write primitiveTriples
primitiveTriples' l = pt (3,4,5) where
  pt x = x : concatMap pt (filter l $ branchTriple <*> [x])

-- usage written with list comprehension
usage' n = [t | (a,b,c) <- primTriples n (3,4,5), t <- [(a*k, b*k, c*k) | k <- [1..div n c]]] ; primTriples n p = p : [t | q <- branchTriple <*> [p], thrd q <= n, t <- primTriples n q] ; thrd (a,b,c) =

-- TODO  :https://rosettacode.org/wiki/Pythagorean_triples
