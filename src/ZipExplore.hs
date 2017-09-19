
module ZipExplore where

import Data.Tuple.Select

-- [1, 2, 3, 4] --> [(1, 2), (2, 3), (3, 4)]

zipListElements :: [Int] -> [(Int, Int)]
zipListElements xs = zip xs $ tail xs

zipListElements' = zip <*> tail

cnv :: [Integer] -> [(Integer, Integer)]
cnv [] = []
cnv (k:v:t) = (k, v) : cnv t

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

