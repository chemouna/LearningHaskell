module ConstUsage where

import Data.List
import Data.Foldable

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

