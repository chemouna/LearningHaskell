
module TrivialMonad where

import Text.Show
import Control.Monad

data W a = W a deriving Show

return' :: a -> W a
return' x = W x

-- we need a way to manipulate the wrapped data while keeping it wrapped :
fmap' :: (a -> b) -> (W a -> W b)
fmap' f (W x) = W (f x)

-- we want to apply operation multiple times without the user being able to unwrap
-- so we provide a fn that does unwraping and applying fn then it gives back the wrapped result
bind' :: (a -> W b) -> (W a -> W b)
bind' f (W x) = (f x)

-- now we can do d = bind f (bind  f (f 1))
-- bind is more general then fmap : fmap = bind . (return f)

-- compw :: W a -> (a -> W b) -> W b -- equivalent to >>= 
-- compw = _

-- -- ex 1/
-- g :: Int -> W Int -> W Int
-- g x y = y compw (return' . (+x))


-- h ::  W Int -> W Int -> W Int
-- h x y = y compw (+x) 

