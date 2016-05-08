module StateMonad where

import Control.Monad
import Control.Monad.State

-- some State Monad examples

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

swap :: State Stack ()
swap = do a <- pop
          b <- pop
          push a
          push b

dup :: State Stack ()
dup = do a <- pop
         push a
         push a

drop :: State Stack ()
drop = do pop
          a <- pop
          push a
