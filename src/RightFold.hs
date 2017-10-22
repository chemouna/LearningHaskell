
module RightFold where

-- Exploring right fold

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' combine base [] = base
foldr' combine base (x:xs) = combine x (foldr combine base xs)

sum'' = foldr' (+) 0
product'' = foldr' (*) 1


