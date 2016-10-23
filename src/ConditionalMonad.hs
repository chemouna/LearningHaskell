
module ConditionalMonad where

import Control.Conditional
import Control.Monad

(==:) :: (Eq a,Monad m) => m a -> m a -> m Bool
(==:) = liftM2 (==)

main = ifM (getLine ==: getLine) (print "hit") (print "miss")
