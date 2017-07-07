{-# LANGUAGE TemplateHaskell #-}
module ConstUsage where

import Data.List
import Data.Foldable
import Data.Functor

-- const
-- can be use for passing higher order fns when we don't need
-- all their flexibility , like
-- x >> y = x >>= const y

-- which is cleaner then x >> y = x >>= \_ -> y
-- (>>) = (. const) . (>>=) -- in pointfree

-- can be used on a single argument to yield a function where one is needed like :
-- map (const 42 [1..10])

-- const can be used to get rid of lambda application that takes 2 args uses one
-- and throws away the second, f.ex :
length' :: Foldable t => t a -> Int
length' = foldr (\_ acc -> 1 + acc) 0

-- can instead be written more elegantly with const instead of lambda :
length'' :: Foldable t => t a -> Int
length'' = foldr (const (+1)) 0

-- another use case for const is for saying i have a functor with something boring in it
-- and i want another interesting thing in it instead

-- 42 <$ Just "boring thing"
-- 42 <$ Nothing
-- "cool" <$ ["uncool", "uninsteresting", "boring"]

-- an interseting property about const :
-- const id = flip const



-- TODO: look into Const functor + forall + 

