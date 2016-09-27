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


id' :: a -> a
-- id' = \x -> x
id' x = x

prop_identity x = id' x == x

const' :: a -> b -> a
const' x y = x

prop_const x y = const' x y == x

apply :: (a -> b) -> a -> b
apply f x = f x

prop_app x = f `apply` x == f x
  where f = (*2)

compose :: (b -> c) -> (a -> b) -> (a -> c)
-- compose f g = f (\x -> _  x)
compose f g x = f (g x)

prop_compose x = (f `compose` g) x == f (g x)
  where f = (*2)
        g = (+1)


xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor False False = False
xor True True = False

prop_xor x y =  x `xor` y == (x /= y)


not' :: Bool -> Bool
not' False = True
not' True = False

prop_bool_not_inequality b = not' b /= b
prop_bool_not_involutive b = not' (not' b) == b

or :: Bool -> Bool -> Bool
or True False = False
or False True = True
or False False = False
or True True = True

and :: Bool -> Bool -> Bool
and True False = False
and False True = False
and False False = False
and True True = True


prop_bool_deMorgan_1 x y = (not' x) `and` (not y) == not (x `or` y)
prop_bool_deMorgan_2 x y = (not' x) `or` (not y) == not (x `and` y)

