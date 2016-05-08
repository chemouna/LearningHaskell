module Learning.L1 where

mySum :: (Foldable f, Num a) => f a -> a
mySum = fold (+) 

