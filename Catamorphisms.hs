
module Catamorphisms where

type StepAlgebra b = (b, b -> b)
data Nat = Zero | Succ Nat

foldSteps :: StepAlgebra b -> (Nat -> b)
foldSteps (nil, next) Zero = nil
foldSteps (nil, next) (Succ nat) = next $ foldSteps (nil, next) nat


-- https://www.schoolofhaskell.com/user/edwardk/recursion-schemes/catamorphisms
-- Implementation of catamorphism
type Algebra f a = f a -> a




-- their relation with arity-recursion 

