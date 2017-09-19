
module ZipExplore where

-- [1, 2, 3, 4] --> [(1, 2), (2, 3), (3, 4)]

zipListElements :: [Int] -> [(Int, Int)]
zipListElements xs = zip xs $ tail xs

zipListElements' = zip <*> tail

cnv :: [Int] -> [(Int, Int)]
cnv [] = []
cnv (k:v:t) = (k, v) : cnv t

