
module PythagoreanTriples where

-- get pythagorean triples without symetrical values
pyth_trip_non_sym :: Int -> [(Int, Int, Int)]
pyth_trip_non_sym n = [(a, b, c) | a <- [1..n], b <- [1..a], c <- [1..n], (a^2) + (b^2) == (c^2)]

-- an even more efficient way
pyth_trip_non_sym2 :: Int -> [(Int, Int, Int)]
pyth_trip_non_sym2 n = [(a, b, a^2 + b^2) | a <- [1..n], b <- [1..a], isSquare (a^2 + b^2)]

isSquare x = let x' = truncate $ sqrt (fromIntegral x :: Double) in x'*x' == x

-- using Tree of primitive pythagorean triples


-- TODO  :https://rosettacode.org/wiki/Pythagorean_triples 
