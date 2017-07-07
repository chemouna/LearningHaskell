module Learning.StateMonad where

fact3 :: Integer -> State Integer Integer
fact3 0 = return 1
fact3 n = do
  let n' = n-1
  modify (+1)
  m <- fact3 n'
  let r = n*m
  modify (+1)
  return r

ex3 = runState (fact3 10) 0