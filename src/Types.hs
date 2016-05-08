module Types where

type Point = (Int, Int) -- an alias here for a pair of ints
data Triangle = Triangle Point Point Point deriving Show
data Square = Square Point Point Point Point deriving Show

class Shape a where
  rotate :: a -> a
  simple :: a

-- we can make Triangle of class type shape later on 
instance Shape Triangle where
  rotate (Triangle x y z) = Triangle z x y
  simple = Triangle (0, 0) (1, 0) (0, 1) 

instance Shape Square where
  rotate (Square w x y z) = Square z w x y
  simple = Square (0, 0) (1, 0) (1, 1) (0, 1)

  

