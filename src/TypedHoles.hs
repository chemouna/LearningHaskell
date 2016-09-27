{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}


module TypedHoles where

import Control.Monad
import Control.Applicative
import           Test.QuickCheck

data Free f a = Pure a | Free (f (Free f a))

-- now let's write an instance Functor for it :
-- instance Functor f => Monad (Free f) where
--   return a = Pure a
--   Pure a >>= f = f a
--   Free f >>= g = Free (fmap (>>= g) f)


-- instance Applicative f => Monad (Free f) where
--   pure = return
--   (<*>) = ap

--

id' :: a -> a
-- id' = \x -> x
id' x = x

prop_identity x = id' x == x

const' :: a -> b -> a
const' = \x -> y -> x

prop_const x y = const' x y == x
