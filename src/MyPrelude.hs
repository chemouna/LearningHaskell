
module MyPrelude where

-- my attempt at implementing some Prelude functions

myindex :: [a] -> Int -> a
xs `myindex` k | k < 0 = error "Cannot get a value for a negative index"
[] `myindex` _         =  error "Prelude.!!: index too large"
(x:_) `myindex` 0      = x
(_:xs) `myindex` k     = xs `myindex` (k - 1)
