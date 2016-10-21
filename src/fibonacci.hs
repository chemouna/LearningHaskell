module Fibonacci where

import Control.Monad.State


-- nth fib number
nthfib :: Int -> Int
nthfib 0 = 0
nthfib 1 = 1
nthfib n = nthfib (n - 1) + nthfib (n - 2)

-- monadic fib
fibM n = flip evalState (0, 1) $ do
  forM [0..(n - 1)] $ \_ -> do
    (a,b) <- get
    put (b, a+b)
  (a,b) <- get
  return a
