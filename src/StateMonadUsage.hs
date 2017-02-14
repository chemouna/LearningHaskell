
module StateMonadUsage where

import Control.Monad.State.Lazy

-- TODO solve Euler 115 with the state Monad


len2State :: String -> State Int Bool
len2State s = return (length s == 2)

-- continue 
-- http://stackoverflow.com/questions/1956518/state-monad-sequences-of-random-numbers-and-monadic-code?rq=1

--

tick :: State Int Int
tick = do n <- get
          put (n+1)
          return n

plusOne :: Int -> Int
plusOne n = execState tick n

plus :: Int -> Int -> Int
plus n x = execState (sequence $ replicate n tick) x
