
module Lifting where

data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

