
module MyPrelude where

-- my attempt at implementing some Prelude functions

myindex :: [a] -> Int -> a
xs `myindex` k | k < 0 = error "Cannot get a value for a negative index"
[] `myindex` _         =  error "Prelude.!!: index too large"
(x:_) `myindex` 0      = x
(_:xs) `myindex` k     = xs `myindex` (k - 1)


myDeleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
myDeleteBy _ _ [] = []
myDeleteBy eq v (x:xs) = if v `eq` x then xs else x : myDeleteBy eq v xs
